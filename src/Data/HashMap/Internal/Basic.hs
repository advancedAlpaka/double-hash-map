{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Data.HashMap.Internal.Basic where

import qualified Data.HashMap.Internal.Base as HB
import qualified Data.HashMap.Internal.Class as HC
import Data.List (sortBy)
import Data.Maybe (fromMaybe, isNothing, fromJust)
import qualified Data.Primitive.Array as A
import GHC.Stack (HasCallStack)
import Data.Bifunctor (second)
import Data.Foldable (Foldable(foldr'), forM_)
import Data.Functor ((<&>))
import Data.HashMap.Internal.Class (DoubleHashable(..))
import Data.HashMap.Internal.Array (indA, Arr (..))
import Data.HashMap.Internal.Base (insertHelper)
import Data.STRef (newSTRef, readSTRef, modifySTRef)
import Control.Monad.ST (runST)
import Data.Bits (FiniteBits(..), Bits (..))
import qualified Data.Primitive.SmallArray as SA
import Data.Ratio ((%))

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
      else foldr' (\case
              Just (Just (k, v)) -> HB.resizeInsertWith (f $ HC.unhashed k) k v
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
toList HB.Null = []
toList (HB.HashMap size v) = concatMap (\case
    Just (Just (k, v)) -> [(HC.unhashed k, v)]
    _ -> []) v

keys :: HB.HashMap k v -> [k]
keys = Prelude.map fst . toList

elems :: HB.HashMap k v -> [v]
elems = Prelude.map snd . toList

toAscList :: (Ord k) =>  HB.HashMap k v -> [(k, v)]
toAscList  = sortBy (\(k1, _) (k2, _) -> compare k1 k2) . toList

foldMapWithKey :: (Monoid m) => (k -> v -> m) -> HB.HashMap k v -> m
foldMapWithKey _ HB.Null = mempty 
foldMapWithKey f (HB.HashMap _ v) = foldMap (\case
    Just (Just (k, v)) -> f (HC.unhashed k) v
    _ -> mempty) v

foldrWithKey :: (k -> v -> a -> a) -> a -> HB.HashMap k v -> a
foldrWithKey _ z HB.Null = z 
foldrWithKey f a (HB.HashMap _ v) = foldr (\case
    Just (Just (k, v)) -> f (HC.unhashed k) v
    _ -> id) a v

foldrWithKey' :: (k -> v -> a -> a) -> a -> HB.HashMap k v -> a
foldrWithKey' _ z HB.Null = z 
foldrWithKey' f a (HB.HashMap _ v) = foldr' (\case
    Just (Just (k, v)) -> f (HC.unhashed k) v
    _ -> id) a v

upToBase2 :: Int -> Int
upToBase2 x = bit $ finiteBitSize x - countLeadingZeros x + 1

filterWithKey :: DoubleHashable k => (k -> v -> Bool) -> HB.HashMap k v -> HB.HashMap k v
filterWithKey _ HB.Null = HB.Null
filterWithKey f (HB.HashMap size v) = if newCap == oldCap
    then HB.HashMap (size - deleted) resV
    else HB.resize (HB.ToSize newCap) $ HB.HashMap (size - deleted) resV
  where
    newCap = upToBase2 $ size - deleted
    oldCap = length v
    (deleted, resV) = runST $ case v of
      A av -> do
        avm <- A.unsafeThawArray av
        count <- newSTRef (0 :: Int)
        forM_ [0..oldCap] $ \i -> do
          let p = A.indexArray av i
          case p of
            Just (Just (key', val')) -> if f (HC.unhashed key') val'
              then return ()
              else do
                A.writeArray avm i $ Just Nothing
                modifySTRef count (+1)
            _ -> return ()
        c' <- readSTRef count
        v' <- A.unsafeFreezeArray avm
        return (c', A v')
      SA sav -> do
        savm <- SA.unsafeThawSmallArray sav
        count <- newSTRef (0 :: Int)
        forM_ [0..oldCap] $ \i -> do
          let p = SA.indexSmallArray sav i
          case p of
            Just (Just (key', val')) -> if f (HC.unhashed key') val'
              then return ()
              else do
                SA.writeSmallArray savm i $ Just Nothing
                modifySTRef count (+1)
            _ -> return ()
        c' <- readSTRef count
        v' <- SA.unsafeFreezeSmallArray savm
        return (c', SA v')

filter :: DoubleHashable k => (v -> Bool) -> HB.HashMap k v -> HB.HashMap k v
filter f = filterWithKey (const f)

mapMaybeWithKey :: DoubleHashable k => (k -> v -> Maybe u) -> HB.HashMap k v -> HB.HashMap k u
mapMaybeWithKey _ HB.Null = HB.Null
mapMaybeWithKey f h@(HB.HashMap size v) = if newCap == oldCap
    then HB.HashMap (size - deleted) resV
    else HB.resize (HB.ToSize newCap) $ HB.HashMap (size - deleted) resV
  where
    newCap = upToBase2 $ size - deleted
    oldCap = length v
    (deleted, resV) = runST $ case v of
      A av -> do
        avm <- A.unsafeThawArray av
        navm <- A.newArray oldCap Nothing
        count <- newSTRef (0 :: Int)
        forM_ [0..oldCap] $ \i -> do
          let p = A.indexArray av i
          case p of
            Just (Just (key', val')) -> case f (HC.unhashed key') val' of
              Just newV -> do
                A.writeArray navm i $ Just $ Just (key', newV)
              Nothing -> do
                A.writeArray navm i $ Just Nothing
                modifySTRef count (+1)
            _ -> return ()
        c' <- readSTRef count
        v' <- A.unsafeFreezeArray navm
        return (c', A v')
      SA sav -> do
        savm <- SA.unsafeThawSmallArray sav
        nsavm <- SA.newSmallArray oldCap Nothing
        count <- newSTRef (0 :: Int)
        forM_ [0..oldCap] $ \i -> do
          let p = SA.indexSmallArray sav i
          case p of
            Just (Just (key', val')) -> case f (HC.unhashed key') val' of
              Just newV -> do
                SA.writeSmallArray nsavm i $ Just $ Just (key', newV)
              Nothing -> do
                SA.writeSmallArray nsavm i $ Just Nothing
                modifySTRef count (+1)
            _ -> return ()
        c' <- readSTRef count
        v' <- SA.unsafeFreezeSmallArray nsavm
        return (c', SA v')

mapMaybe :: DoubleHashable k => (a -> Maybe b) -> HB.HashMap k a -> HB.HashMap k b
mapMaybe f = mapMaybeWithKey (const f)

--traverseWithKey :: Applicative f => (k -> v1 -> f v2) -> HB.HashMap k v1 -> f (HB.HashMap k v2)
