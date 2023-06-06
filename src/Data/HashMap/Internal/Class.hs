
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.HashMap.Internal.Class where
import Data.Hashable (Hashable (..))
import System.CPUTime (getCPUTime)
import GHC.IO (unsafePerformIO)
import System.Random
import System.Random.Stateful (Uniform(uniformM), globalStdGen)
import qualified Data.Digest.Murmur as DM
import qualified Data.Hashable as M
import Data.String (IsString (fromString))
import Control.DeepSeq (NFData (rnf))
import Data.Functor.Classes (Eq1 (liftEq), Ord1 (liftCompare), Show1 (liftShowsPrec))

(salt1, salt2) = (-8794413902834329699,-707115575054721689) 
  {-unsafePerformIO $ do
    let g = globalStdGen
    s1 <- uniformM g
    s2 <- uniformM g
    return (s1, s2)-}

class (Eq a) => DoubleHashable a where
    hash1WithSalt :: Int -> a -> Int
    hash2WithSalt :: Int -> a -> Int

hash1 :: (DoubleHashable a) => a -> Int
hash1 = hash1WithSalt salt1
{-# INLINE hash1 #-}

hash2 :: (DoubleHashable a) => a -> Int
hash2 = hash2WithSalt salt2
{-# INLINE hash2 #-}

-- | A hashable value along with the result of the 'hash' function.
data Hashed a = Hashed a 
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !Int

-- | Wrap a hashable value, caching the 'hash' function result.
hashed :: DoubleHashable a => a -> Hashed a
hashed a = Hashed a (hash1 a) (hash2 a)
{-# INLINE hashed #-}

-- | Unwrap hashed value.
unhashed :: Hashed a -> a
unhashed (Hashed a _ _) = a
{-# INLINE unhashed #-}

-- | 'hash' has 'Eq' requirement.
--
-- @since 1.4.0.0
hashedHash1 :: Hashed a -> Int
hashedHash1 (Hashed _ h _) = h

hashedHash2 :: Hashed a -> Int
hashedHash2 (Hashed _ _ h) = h

-- | Uses precomputed hash to detect inequality faster
instance Eq a => Eq (Hashed a) where
  Hashed a h1a h2a == Hashed b h1b h2b = h1a == h1b && h2a == h2b && a == b

instance Ord a => Ord (Hashed a) where
  Hashed a _ _ `compare` Hashed b _ _ = a `compare` b

instance Show a => Show (Hashed a) where
  showsPrec d (Hashed a _ _) = showParen (d > 10) $
    showString "hashed" . showChar ' ' . showsPrec 11 a

--instance DoubleHashable a => DoubleHashable (Hashed a) where
--  hash1WithSalt = defaultHashWithSalt
  
instance (IsString a, DoubleHashable a) => IsString (Hashed a) where
  fromString s = let r = fromString s in Hashed r (hash1 r) (hash2 r)

instance Foldable Hashed where
  foldMap f (Hashed a _ _) = f a
  foldr f acc (Hashed a _ _) = f a acc

instance NFData a => NFData (Hashed a) where
  rnf = rnf . unhashed

-- | 'Hashed' cannot be 'Functor'
mapHashed :: DoubleHashable b => (a -> b) -> Hashed a -> Hashed b
mapHashed f (Hashed a _ _) = hashed (f a)

-- | 'Hashed' cannot be 'Traversable'
traverseHashed :: (DoubleHashable b, Functor f) => (a -> f b) -> Hashed a -> f (Hashed b)
traverseHashed f (Hashed a _ _) = fmap hashed (f a)

-- instances for @Data.Functor.Classes@ higher rank typeclasses
-- in base-4.9 and onward.
instance Eq1 Hashed where
  liftEq f (Hashed a h1a h2a) (Hashed b h1b h2b) = h1a == h1b && h2a == h2b && f a b

instance Ord1 Hashed where
  liftCompare f (Hashed a _ _) (Hashed b _ _) = f a b

instance Show1 Hashed where
  liftShowsPrec sp _ d (Hashed a _ _) = showParen (d > 10) $
    showString "hashed " . sp 11 a

{-instance (DM.Hashable a, Eq a) => DoubleHashable a where
    hash1WithSalt = fromEnum . DM.hashWithSalt salt1
    hash2WithSalt = fromEnum . DM.hashWithSalt salt2-}
{-instance (M.Hashable a, Eq a) => DoubleHashable a where
    hash1WithSalt = M.hashWithSalt
    hash2WithSalt = M.hashWithSalt-}