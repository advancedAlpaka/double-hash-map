{-# LANGUAGE FlexibleContexts #-}
module Main(main) where

import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as A
import Data.ByteString (ByteString, pack)
import qualified Data.Hashable as HH
import Data.HashMap (DoubleHashable)
import qualified Data.HashMap as H
import qualified Data.HashMap.Lazy as HL
import qualified Data.HashMap.Strict as HS
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as I
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import System.Random.MWC
import Test.Tasty.Bench (bench, bgroup, defaultMain, env, whnf)

type V = U.Vector Int
type B = V.Vector ByteString
type K = V.Vector A.Key

numbers :: IO (V, V, V)
numbers = do
  gen <- createSystemRandom
  random <- uniformVector gen 40000
  let sorted    = G.modify I.sort random
      revsorted = G.reverse sorted
  return (random, sorted, revsorted)

strings :: IO (B, B, B)
strings = do
  gen <- createSystemRandom
  random <- V.replicateM 10000 $
      (pack . U.toList) `fmap` (uniformVector gen =<< uniformR (1,16) gen)
  let sorted    = G.modify I.sort random
      revsorted = G.reverse sorted
  return (random, sorted, revsorted)

keys :: IO (K, K, K)
keys = do
  gen <- createSystemRandom
  random <- V.replicateM 10000 $
      (K.fromString . U.toList) `fmap` (uniformVector gen =<< uniformR (1,16) gen)
  let sorted    = G.modify I.sort random
      revsorted = G.reverse sorted
  return (random, sorted, revsorted)

value :: Int
value = 31337

hashmap :: (G.Vector v k, H.DoubleHashable k) => v k -> H.HashMap k Int
hashmap = G.foldl' (\m k -> H.insert k value m) H.null

keymap :: (G.Vector v A.Key) => v A.Key -> A.KeyMap Int
keymap = G.foldl' (\m k -> A.insert k value m) A.empty

intmap :: G.Vector v Int => v Int -> IM.IntMap Int
intmap = G.foldl' (\ m k -> IM.insert k value m) IM.empty

mmap :: (G.Vector v k, Ord k) => v k -> M.Map k Int
mmap = G.foldl' (\ m k -> M.insert k value m) M.empty

oldlazyhashmap :: (G.Vector v k, HH.Hashable k) => v k -> HL.HashMap k Int
oldlazyhashmap = G.foldl' (\m k -> HL.insert k value m) HL.empty

oldstricthashmap :: (G.Vector v k, HH.Hashable k) => v k -> HS.HashMap k Int
oldstricthashmap = G.foldl' (\m k -> HS.insert k value m) HS.empty

instance DoubleHashable ByteString
instance DoubleHashable A.Key

main :: IO ()
main =  defaultMain [
          env numbers $ \ ~(random,sorted,revsorted) ->
          bgroup "Int" [
            bgroup "IntMap" [
             bench "sorted"    $ whnf intmap sorted
           , bench "random"    $ whnf intmap random
           , bench "revsorted" $ whnf intmap revsorted
           ]
        , bgroup "Map" [
             bench "sorted"    $ whnf mmap sorted
           , bench "random"    $ whnf mmap random
           , bench "revsorted" $ whnf mmap revsorted
           ]
        , bgroup "HashMap" [
             bench "sorted"    $ whnf hashmap sorted
           , bench "random"    $ whnf hashmap random
           , bench "revsorted" $ whnf hashmap revsorted
           ]
        , bgroup "HashMapLazy" [
             bench "sorted"    $ whnf oldlazyhashmap sorted
           , bench "random"    $ whnf oldlazyhashmap random
           , bench "revsorted" $ whnf oldlazyhashmap revsorted
           ]
        , bgroup "HashMapStrict" [
             bench "sorted"    $ whnf oldstricthashmap sorted
           , bench "random"    $ whnf oldstricthashmap random
           , bench "revsorted" $ whnf oldstricthashmap revsorted
           ]
        , bgroup "IntMap" [
             bench "sorted"    $ whnf intmap sorted
           , bench "random"    $ whnf intmap random
           , bench "revsorted" $ whnf intmap revsorted
           ]  
         ]
       , env strings $ \ ~(random,sorted,revsorted) ->
         bgroup "ByteString" [
           bgroup "Map" [
             bench "sorted"    $ whnf mmap sorted
           , bench "random"    $ whnf mmap random
           , bench "revsorted" $ whnf mmap revsorted
           ]
         , bgroup "HashMap" [
             bench "sorted"    $ whnf hashmap sorted
           , bench "random"    $ whnf hashmap random
           , bench "revsorted" $ whnf hashmap revsorted
           ]
        , bgroup "HashMapLazy" [
             bench "sorted"    $ whnf oldlazyhashmap sorted
           , bench "random"    $ whnf oldlazyhashmap random
           , bench "revsorted" $ whnf oldlazyhashmap revsorted
           ]
        , bgroup "HashMapStrict" [
             bench "sorted"    $ whnf oldstricthashmap sorted
           , bench "random"    $ whnf oldstricthashmap random
           , bench "revsorted" $ whnf oldstricthashmap revsorted
           ]
         ]
        , env keys $ \ ~(random,sorted,revsorted) ->
          bgroup "Key" [
            bgroup "KeyMap" [
             bench "sorted"    $ whnf keymap sorted
           , bench "random"    $ whnf keymap random
           , bench "revsorted" $ whnf keymap revsorted
           ]
         , bgroup "HashMap" [
             bench "sorted"    $ whnf hashmap sorted
           , bench "random"    $ whnf hashmap random
           , bench "revsorted" $ whnf hashmap revsorted
           ]
       ]]
