{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main(main) where

import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as A
--import Data.ByteString (ByteString, pack)
import qualified Data.Hashable as HH
import qualified Data.HashMap as H
import qualified Data.HashMap.Lazy as HL
--import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as I
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

import System.Random.MWC
import Test.Tasty.Bench (bench, bgroup, defaultMain, env, whnf)
import Data.HashMap.Internal.Debug (salts)
import Data.Hashable (Hashable(hashWithSalt))
import Control.DeepSeq (NFData)

--type V = U.Vector Int
--type B = V.Vector ByteString
type K = V.Vector Key

{-numbers :: IO (V, V, V)
numbers = do
  gen <- createSystemRandom
  random <- uniformVector gen 100_000
  let sorted    = G.modify I.sort random
      revsorted = G.reverse sorted
  return (random, sorted, revsorted)

strings :: IO (B, B, B)
strings = do
  gen <- createSystemRandom
  random <- V.replicateM 500_000 $
      (pack . U.toList) `fmap` (uniformVector gen =<< uniformR (1,16) gen)
  let sorted    = G.modify I.sort random
      revsorted = G.reverse sorted
  return (random, sorted, revsorted)-}

keys :: IO (K, K, K)
keys = do
  gen <- createSystemRandom
  random <- V.replicateM 1_000_000 $
      (K . K.fromString . U.toList) `fmap` (uniformVector gen =<< uniformR (1,16) gen)
  let sorted    = G.modify I.sort random
      revsorted = G.reverse sorted
  return (random, sorted, revsorted)

value :: Int
value = 31_337

hashmap :: (G.Vector v k, H.DoubleHashable k) => v k -> H.HashMap k Int
hashmap = G.foldl' (\m k -> H.insert k value m) H.null

keymap :: (G.Vector v Key) => v Key -> A.KeyMap Int
keymap = G.foldl' (\m k -> A.insert (unK k) value m) A.empty

--mmap :: (G.Vector v k, Ord k) => v k -> M.Map k Int
--mmap = G.foldl' (\ m k -> M.insert k value m) M.empty

oldlazyhashmap :: (G.Vector v k, HH.Hashable k) => v k -> HL.HashMap k Int
oldlazyhashmap = G.foldl' (\m k -> HL.insert k value m) HL.empty

--instance DoubleHashable Int
--instance DoubleHashable ByteString

newtype Key = K { unK :: A.Key }
  deriving (Eq, Ord, Show, NFData, Hashable)
  
instance H.DoubleHashable Key where
  hash1WithSalt = hashWithSalt
--  hash1WithSalt salt = fromEnum . hashWithSalt (toEnum (salt `mod` 0xFFFF_FFFF)) . K.toText . unK
  {-# INLINE hash1WithSalt #-}

  hash2WithSalt = hashWithSalt
--  hash2WithSalt salt = fromEnum . hashWithSalt (toEnum (salt `mod` 0xFFFF_FFFF)) . K.toText . unK
  {-# INLINE hash2WithSalt #-}

main :: IO ()
main =  defaultMain [
        env keys $ \ ~(random,sorted,revsorted) ->
          bgroup "Key" [
            bgroup "KeyMap" [
             bench "sorted"    $ whnf keymap sorted
           , bench "random"    $ whnf keymap random
           , bench "revsorted" $ whnf keymap revsorted
           ]
         , bgroup ("HashMap " ++ show salts)  [
             bench "sorted"    $ whnf hashmap sorted
           , bench "random"    $ whnf hashmap random
           , bench "revsorted" $ whnf hashmap revsorted
           ]
        , bgroup "HashMapLazy" [
             bench "sorted"    $ whnf oldlazyhashmap sorted
           , bench "random"    $ whnf oldlazyhashmap random
           , bench "revsorted" $ whnf oldlazyhashmap revsorted
           ]
        ]
      ]
