{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase   #-}

module Data.HashMap.Internal.Instance where

import Control.DeepSeq (NFData (..))
import Data.Bifunctor (Bifunctor (..))
import Data.HashMap.Internal.Base (HashMap (..), defH, insert, size)
import qualified Data.HashMap.Internal.Base as HB (null)
import Data.HashMap.Internal.Class (DoubleHashable)
import Data.List (intercalate)
import qualified Data.Vector as V

instance (Show k, Show v) => Show (HashMap k v) where
  show :: HashMap k v -> String
  show (HashMap _ _ _ size v) =
    "[" ++ intercalate "," (V.toList( V.mapMaybe (\case
                    Just (Just (k, v)) -> Just ("(" ++ show k ++ "," ++ show v ++ ")")
                    _ -> Nothing) v)) ++ "]"

instance Foldable (HashMap k) where
  foldr :: (a -> b -> b) -> b -> HashMap k a -> b
  foldr f z (HashMap _ _ _ _ v) =
    V.foldr (\case
          Just (Just (_, v)) -> f v
          _ -> id) z v

instance Functor (HashMap k) where
  fmap :: (a -> b) -> HashMap k a -> HashMap k b
  fmap f h@(HashMap _ _ _ size v) = defH h size (V.map (fmap (fmap (second f))) v)

instance (Eq k, Eq v) => Eq (HashMap k v) where
  (==) :: (Eq k, Eq v) => HashMap k v -> HashMap k v -> Bool
  (==) h1 h2 = size h1 == size h2

instance (NFData k, NFData v) => NFData (HashMap k v) where
  rnf :: (NFData k, NFData v) => HashMap k v -> ()
  rnf (HashMap _ _ _ size v) = rnf size `seq` rnf v

instance (DoubleHashable k) => Semigroup (HashMap k v) where
  (<>) :: (DoubleHashable k) => HashMap k v -> HashMap k v -> HashMap k v
  (<>) h1@(HashMap _ _ _ size1 v1) h2@(HashMap _ _ _ size2 v2) =
    if size1 < size2
      then h2 <> h1
      else foldr ( \case
              Just (Just (k, v)) -> insert k v
              _ -> id) h1 v2

instance (DoubleHashable k) => Monoid (HashMap k v) where
  mempty :: DoubleHashable k => HashMap k v
  mempty = HB.null
