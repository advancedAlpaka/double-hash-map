
{-# LANGUAGE FlexibleInstances #-}
module Data.HashMap.Internal.Class where
import Data.Hashable (Hashable (..))
import System.CPUTime (getCPUTime)
import GHC.IO (unsafePerformIO)

{-# NOINLINE salt #-}
salt = fromInteger $ unsafePerformIO getCPUTime

class (Hashable a) => DoubleHashable a where
    hash2 :: a -> Int
    hash2 = hashWithSalt salt

instance DoubleHashable Int
instance DoubleHashable Char
instance DoubleHashable Bool
instance DoubleHashable Double
instance DoubleHashable Float
instance DoubleHashable Integer
instance DoubleHashable String