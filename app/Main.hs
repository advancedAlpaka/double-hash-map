{-# LANGUAGE NumericUnderscores #-}
module Main where

import qualified Data.Aeson.Key as K
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import System.Random.MWC
import Control.Monad (forM_)
import qualified Data.HashMap as H
import qualified Data.Vector.Generic as G
import Data.Hashable
import Data.HashMap.Internal.Debug (showDebug)
import Control.DeepSeq (NFData(rnf))
import Data.Primitive.Array

instance H.DoubleHashable K.Key where
  hash1WithSalt = hashWithSalt
--  hash1WithSalt salt = fromEnum . hashWithSalt (toEnum (salt `mod` 0xFFFF_FFFF)) . K.toText
  {-# INLINE hash1WithSalt #-}

  hash2WithSalt = hashWithSalt
--  hash2WithSalt salt = fromEnum . hashWithSalt (toEnum (salt `mod` 0xFFFF_FFFF)) . K.toText
  {-# INLINE hash2WithSalt #-}

hashmap :: (G.Vector v k, H.DoubleHashable k) => v k -> H.HashMap k Int
hashmap = G.foldl' (\m k -> H.insert k 31_337 m) H.null

main :: IO ()
main = do
  gen <- createSystemRandom
  random <- V.replicateM 1_000_000 $
      (K.fromString . U.toList) `fmap` (uniformVector gen =<< uniformR (1,16) gen)
  let h = hashmap random
  print h