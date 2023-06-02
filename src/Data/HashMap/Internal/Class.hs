
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
    hash1 :: a -> Int
    hash1 = hashWithSalt salt1
    hash2 :: a -> Int
    hash2 v = 1 + 2 * hashWithSalt salt2 v

instance DoubleHashable Int
instance DoubleHashable String
--instance (Hashable a) => DoubleHashable a