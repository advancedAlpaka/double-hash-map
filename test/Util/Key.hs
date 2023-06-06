{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}

module Util.Key (Key(..), keyToInt, incKey, collisionAtHash) where

import Data.Bits       (bit, (.&.))
import Data.Hashable (Hashable (hashWithSalt))
import Data.Word       (Word16)
import GHC.Generics    (Generic)
import Test.QuickCheck (Arbitrary (..), CoArbitrary (..), Function, Gen, Large)

import qualified Data.HashMap as HM
import qualified Test.QuickCheck as QC

-- Key type that generates more hash collisions.
data Key = K
  { hash1 :: !Int
  , hash2 :: !Int
    -- ^ The hash of the key
  , _x :: !SmallSum
    -- ^ Additional data, so we can have collisions for any hash
  } deriving (Eq, Ord, Read, Show, Generic, Function, CoArbitrary)

instance Hashable Key where
  hashWithSalt _ (K h _ _) = h

instance HM.DoubleHashable Key where
  hash1WithSalt _ (K h _ _) = h
  hash2WithSalt _ (K _ h _) = h

data SmallSum = A | B | C | D
  deriving (Eq, Ord, Read, Show, Generic, Enum, Bounded, Function, CoArbitrary)

instance Arbitrary SmallSum where
  arbitrary = QC.arbitraryBoundedEnum
  shrink = shrinkSmallSum

shrinkSmallSum :: SmallSum -> [SmallSum]
shrinkSmallSum A = []
shrinkSmallSum B = [A]
shrinkSmallSum C = [A, B]
shrinkSmallSum D = [A, B, C]

instance Arbitrary Key where
  arbitrary = K <$> arbitraryHash <*> arbitraryHash  <*> arbitrary
  shrink = QC.genericShrink

arbitraryHash :: Gen Int
arbitraryHash = do
  let gens =
        [ (2, fromIntegral . QC.getLarge <$> arbitrary @(Large Word16))
        , (1, QC.getSmall <$> arbitrary)
        , (1, QC.getLarge <$> arbitrary)
        ]
  i <- QC.frequency gens
  moreCollisions' <- QC.elements [moreCollisions, id]
  pure (moreCollisions' i)

-- | Mask out most bits to produce more collisions
moreCollisions :: Int -> Int
moreCollisions w = fromIntegral (w .&. mask)

mask :: Int
mask = sum [bit n | n <- [0, 3, 8, 14, 61]]

keyToInt :: Key -> Int
keyToInt (K h1 h2 x) = h1 * h2 * fromEnum x

incKey :: Key -> Key
incKey (K h1 h2 x) = K (h1 + 1) (h2 + 1) x

-- | 4 colliding keys at a given hash.
collisionAtHash :: Int -> Int -> (Key, Key, Key, Key)
collisionAtHash h1 h2  = (K h1 h2 A, K h1 h2 B, K h1 h2 C, K h1 h2 D)