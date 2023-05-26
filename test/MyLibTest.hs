{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main (main) where

import Data.Function (on)
import Data.Hashable (Hashable(..))
import qualified Data.List as L
import qualified Data.HashMap as M
import Test.QuickCheck (Arbitrary)

-- Key type that generates more hash collisions.
newtype Key = K { unK :: Int }
            deriving (Arbitrary, Eq, Ord, Show)

instance Hashable Key where
    hashWithSalt s k = hashWithSalt s (unK k) `mod` 20

instance M.DoubleHashable Key

main :: IO ()
main = putStrLn "Not Implemented"