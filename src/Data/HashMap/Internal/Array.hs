{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}

module Data.HashMap.Internal.Array where
import qualified Data.Primitive.Array as A
import qualified Data.Primitive.SmallArray as SA
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.Foldable (Foldable(..))
import Data.Hashable (Hashable)

data Arr v =  A {-# UNPACK #-} !(A.Array v)
            | SA {-# UNPACK #-} !(SA.SmallArray v)
              deriving (Generic, NFData, Functor, Show, Eq)

--deriving instance Hashable v => Hashable (Arr v)

instance Foldable Arr where
  fold :: Monoid m => Arr m -> m
  fold (A a) = fold a
  fold (SA a) = fold a
  foldMap :: Monoid m => (a -> m) -> Arr a -> m
  foldMap f (A a) = foldMap f a
  foldMap f (SA a) = foldMap f a
  foldMap' :: Monoid m => (a -> m) -> Arr a -> m
  foldMap' f (A a) = foldMap' f a
  foldMap' f (SA a) = foldMap' f a
  foldr :: (a -> b -> b) -> b -> Arr a -> b
  foldr f z (A a) = foldr f z a
  foldr f z (SA a) = foldr f z a
  foldr' :: (a -> b -> b) -> b -> Arr a -> b
  foldr' f z (A a) = foldr' f z a
  foldr' f z (SA a) = foldr' f z a
  foldl :: (b -> a -> b) -> b -> Arr a -> b
  foldl f z (A a) = foldl f z a
  foldl f z (SA a) = foldl f z a
  foldl' :: (b -> a -> b) -> b -> Arr a -> b
  foldl' f z (A a) = foldl' f z a
  foldl' f z (SA a) = foldl' f z a
  {-# INLINE foldl' #-}
  foldr1 :: (a -> a -> a) -> Arr a -> a
  foldr1 f (A a) = foldr1 f a
  foldr1 f (SA a) = foldr1 f a
  foldl1 :: (a -> a -> a) -> Arr a -> a
  foldl1 f (A a) = foldl1 f a
  foldl1 f (SA a) = foldl1 f a
  toList :: Arr a -> [a]
  toList (A a) = toList a
  toList (SA a) = toList a
  null :: Arr a -> Bool
  null (A a) = null a
  null (SA a) = null a
  length :: Arr a -> Int
  length (A a) = length a
  length (SA a) = length a
  {-# INLINE length #-}

indA :: Arr v -> Int -> v
indA (A a) = A.indexArray a
indA (SA a) = SA.indexSmallArray a
{-# INLINE indA #-}