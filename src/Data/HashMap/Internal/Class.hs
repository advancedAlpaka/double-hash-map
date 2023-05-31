
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.HashMap.Internal.Class where
import Data.Hashable (Hashable (..))
import System.CPUTime (getCPUTime)
import GHC.IO (unsafePerformIO)
import System.Random
import System.Random.Stateful (Uniform(uniformM), globalStdGen)

(salt1, salt2) = unsafePerformIO $ do
    let g = globalStdGen
    s1 <- uniformM g
    s2 <- uniformM g
    return (s1, s2)

salt = fromInteger $ unsafePerformIO getCPUTime

class (Hashable a) => DoubleHashable a where
    hash1 :: Int -> a -> Int
    hash1 _ = hashWithSalt salt1
    hash2 :: Int -> a -> Int
    hash2 size v = (1 + hashWithSalt salt2 v) `mod` (size - 1)

instance DoubleHashable Int

--instance (Hashable a) => DoubleHashable a