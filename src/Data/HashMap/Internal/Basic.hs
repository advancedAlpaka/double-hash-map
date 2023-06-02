{-# LANGUAGE LambdaCase #-}

module Data.HashMap.Internal.Basic where

import qualified Data.HashMap.Internal.Base as HB
import qualified Data.HashMap.Internal.Class as HC
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import qualified Data.Primitive.Array as A
import GHC.Stack (HasCallStack)
import Data.Bifunctor (second)

(!?) :: (HC.DoubleHashable k) => HB.HashMap k v -> k -> Maybe v
(!?) m k = HB.lookup k m

findWithDefault :: (HC.DoubleHashable k)
              => v          -- ^ Default value to return.
              -> k -> HB.HashMap k v -> v
findWithDefault def k m = fromMaybe def (HB.lookup k m)

(!) :: (HC.DoubleHashable k, HasCallStack) => HB.HashMap k v -> k -> v
(!) m k = case HB.lookup k m of
    Nothing -> error "Data.HashMap.Internal.(!): key not found"
    Just v  -> v

infixl 9 !

member :: (HC.DoubleHashable k) => k -> HB.HashMap k v -> Bool
member k m = case HB.lookup k m of
    Nothing -> False
    Just _  -> True

map :: (v -> v') -> HB.HashMap k v -> HB.HashMap k v'
map f h@(HB.HashMap size v) = HB.HashMap size (fmap (fmap (fmap (second f))) v)

union :: (HC.DoubleHashable k) => HB.HashMap k v -> HB.HashMap k v -> HB.HashMap k v
union = unionWith const

unionWith :: (HC.DoubleHashable k) => (v -> v -> v) -> HB.HashMap k v -> HB.HashMap k v -> HB.HashMap k v
unionWith f = unionWithKey (const f)

unionWithKey :: (HC.DoubleHashable k) => (k -> v -> v -> v) -> HB.HashMap k v -> HB.HashMap k v -> HB.HashMap k v
unionWithKey f h1@(HB.HashMap size1 v1) h2@(HB.HashMap size2 v2) =
    if size1 < size2
      then unionWithKey f h2 h1
      else foldr (\case
              Just (Just (k, v)) -> HB.insertWith (f k) k v
              _ -> id) h1 v2

difference :: HC.DoubleHashable k => HB.HashMap k v -> HB.HashMap k v -> HB.HashMap k v
difference = foldrWithKey (\k v h -> HB.delete k h)

intersectionWithKey :: HC.DoubleHashable k => (k -> a -> b -> c) -> HB.HashMap k a -> HB.HashMap k b -> HB.HashMap k c
intersectionWithKey f h1 h2 =  if HB.size h1 > HB.size h2 then intersectionWithKey (\k b' a' -> f k a' b') h2 h1 else foldrWithKey (\k a h ->
    case HB.lookup k h2 of
        Just b' -> HB.insert k (f k a b') h
        Nothing -> h) HB.null h1

intersectionWith :: HC.DoubleHashable k => (a -> b -> c) -> HB.HashMap k a -> HB.HashMap k b -> HB.HashMap k c
intersectionWith = intersectionWithKey . const

intersection :: HC.DoubleHashable k => HB.HashMap k a -> HB.HashMap k b -> HB.HashMap k a
intersection = intersectionWith const

toList :: HB.HashMap k v -> [(k, v)]
toList (HB.HashMap size v) = concatMap (\case
    Just (Just (k, v)) -> [(k, v)]
    _ -> []) v

keys :: HB.HashMap k v -> [k]
keys = Prelude.map fst . toList

elems :: HB.HashMap k v -> [v]
elems = Prelude.map snd . toList

toAscList :: (Ord k) =>  HB.HashMap k v -> [(k, v)]
toAscList  = sortBy (\(k1, _) (k2, _) -> compare k1 k2) . toList

foldMapWithKey :: (Monoid m) => (k -> v -> m) -> HB.HashMap k v -> m
foldMapWithKey f (HB.HashMap _ v) = foldMap (\case
    Just (Just (k, v)) -> f k v
    _ -> mempty) v

foldrWithKey :: (k -> v -> a -> a) -> a -> HB.HashMap k v -> a
foldrWithKey f a (HB.HashMap _ v) = foldr (\case
    Just (Just (k, v)) -> f k v
    _ -> id) a v

{-mapMaybeWithKey :: (k -> v -> Maybe u) -> HB.HashMap k v -> HB.HashMap k u
mapMaybeWithKey f h@(HB.HashMap _ _ _ s v) = _
    where
        (newV, deleted) = foldrWithKey _ _ _ 
        newSize = s - deleted-}