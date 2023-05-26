{-# LANGUAGE LambdaCase #-}

module Data.HashMap.Internal.Basic where

import qualified Data.HashMap as H
import Data.Maybe (fromMaybe)
import GHC.Stack (HasCallStack)
import Data.HashMap.Internal.Instance
import qualified Data.HashMap.Internal.Base as HB
import qualified Data.Vector as V
import Data.List (sortBy)

(!?) :: (H.DoubleHashable k) => H.HashMap k v -> k -> Maybe v
(!?) m k = H.lookup k m

findWithDefault :: (H.DoubleHashable k)
              => v          -- ^ Default value to return.
              -> k -> H.HashMap k v -> v
findWithDefault def k m = fromMaybe def (H.lookup k m)

(!) :: (H.DoubleHashable k, HasCallStack) => H.HashMap k v -> k -> v
(!) m k = case H.lookup k m of
    Nothing -> error "Data.HashMap.Internal.(!): key not found"
    Just v -> v

infixl 9 !

toList :: H.HashMap k v -> [(k, v)]
toList (HB.HashMap _ _ _ size v) = concatMap (\case
    Just (Just (k, v)) -> [(k, v)]
    _ -> []) (V.toList v)

toAscList :: (Ord k) =>  H.HashMap k v -> [(k, v)]
toAscList  = sortBy (\(k1, _) (k2, _) -> compare k1 k2) . toList

foldMapWithKey :: (Monoid m) => (k -> v -> m) -> H.HashMap k v -> m
foldMapWithKey f (HB.HashMap _ _ _ _ v) = V.foldMap (\case
    Just (Just (k, v)) -> f k v
    _ -> mempty) v

foldrWithKey :: (k -> v -> a -> a) -> a -> HB.HashMap k v -> a
foldrWithKey f a (HB.HashMap _ _ _ _ v) = V.foldr (\case
    Just (Just (k, v)) -> f k v
    _ -> id) a v
