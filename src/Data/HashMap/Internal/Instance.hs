{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase   #-}

module Data.HashMap.Internal.Instance where

import Control.DeepSeq (NFData (..))
import Data.Bifunctor (Bifunctor (..))
import Data.HashMap.Internal.Base (HashMap (..), defH, insert, size)
import Data.HashMap.Internal.Basic (union, map)
import qualified Data.HashMap.Internal.Base as HB (null)
import Data.HashMap.Internal.Class (DoubleHashable)
import Data.List (intercalate)
import qualified Data.Vector as V
import Data.Foldable (Foldable(..))

instance (Show k, Show v) => Show (HashMap k v) where
  show :: HashMap k v -> String
  show (HashMap _ _ _ size v) =
    "[" ++ intercalate "," (V.toList ( V.mapMaybe (\case
                    Just (Just (k, v)) -> Just ("(" ++ show k ++ "," ++ show v ++ ")")
                    _ -> Nothing) v)) ++ "]"

fun f = \case
          Just (Just (_, v)) -> f v
          _ -> id
vec (HashMap _ _ _ _ v) = v

instance Foldable (HashMap k) where
  foldr :: (a -> b -> b) -> b -> HashMap k a -> b
  foldr f z = V.foldr (fun f) z . vec
  foldr' :: (a -> b -> b) -> b -> HashMap k a -> b
  foldr' f z = V.foldr' (fun f) z . vec
  foldl :: (b -> a -> b) -> b -> HashMap k a -> b
  foldl = foldr . flip
  foldl' :: (b -> a -> b) -> b -> HashMap k a -> b
  foldl' = foldr . flip

instance Functor (HashMap k) where
  fmap :: (a -> b) -> HashMap k a -> HashMap k b
  fmap = Data.HashMap.Internal.Basic.map

instance (Eq k, Eq v) => Eq (HashMap k v) where
  (==) :: (Eq k, Eq v) => HashMap k v -> HashMap k v -> Bool
  (==) h1 h2 = size h1 == size h2
--    where notEqual h1 h2 = if size h1 > size h2 then notEqual h2 h1 else 

instance (NFData k, NFData v) => NFData (HashMap k v) where
  rnf :: (NFData k, NFData v) => HashMap k v -> ()
  rnf (HashMap _ _ _ size v) = rnf size `seq` rnf v

instance (DoubleHashable k) => Semigroup (HashMap k v) where
  (<>) :: (DoubleHashable k) => HashMap k v -> HashMap k v -> HashMap k v
  (<>) = union

instance (DoubleHashable k) => Monoid (HashMap k v) where
  mempty :: DoubleHashable k => HashMap k v
  mempty = HB.null
