{-# LANGUAGE FlexibleInstances #-}
module Data.HashMap.Internal.Class where
import Data.Hashable (Hashable (..))

class (Hashable a) => DoubleHashable a where
    hash2 :: a -> Int
    hash2 e = 1 + 2 * hashWithSalt 1 e

instance DoubleHashable Int
instance DoubleHashable Char
instance DoubleHashable Bool
instance DoubleHashable Double
instance DoubleHashable Float
instance DoubleHashable Integer
instance DoubleHashable String