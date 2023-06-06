--{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# OPTIONS_GHC -fno-full-laziness -funbox-strict-fields #-}

module Data.HashMap.Internal.Base where

import Control.Exception (throw)
import Data.Bits (Bits (..), FiniteBits (..))
import Data.Foldable ( Foldable(length, foldl, foldl') )
import Data.Hashable (Hashable (hash, hashWithSalt))
import Data.HashMap.Internal.Class (hash1, DoubleHashable)
import qualified Data.HashMap.Internal.Class as HC
import Data.List (intercalate)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Ratio (Ratio, (%))
import qualified Data.Primitive.Array as A
import qualified Data.Primitive.SmallArray as SA
import GHC.Stack (HasCallStack)
import Control.Monad.ST (runST, ST)
import qualified Prelude
import Prelude hiding (null)
import Data.HashMap.Internal.Array (Arr (..), indA)
import qualified GHC.Exts as Exts
import Data.Functor ((<&>))

hash2 :: (DoubleHashable k) => k -> Int
hash2 v = 1 + 2 * HC.hash2 v
{-# INLINE hash2 #-}

data HashMap k v = Null | HashMap {
  size' :: !Int,
  buckets' :: !(Arr (Maybe (Maybe (HC.Hashed k, v)))) }

--type role HashMap nominal representational

size :: HashMap k v -> Int
size Null = 0
size h = size' h

buckets :: HashMap k v -> Arr (Maybe (Maybe (HC.Hashed k, v)))
buckets Null = A A.emptyArray
buckets h = buckets' h

-- Nothing - wasn't setted
-- Just Nothing - was deleted
-- Just (Just (k, v)) - was setted

maxLoadFactor :: Ratio Int
maxLoadFactor = 1 % 2

minLoadFactor :: Ratio Int
minLoadFactor = 1 % 5

key = fst . fromJust . fromJust

val = snd . fromJust . fromJust

null :: HashMap k v
null = Null

singleton :: (DoubleHashable k) => k -> v -> HashMap k v
singleton k = singH (HC.hashed k)
{-# INLINABLE singleton #-}

singH :: (DoubleHashable k) => HC.Hashed k -> v -> HashMap k v
singH k@(HC.Hashed _ h1 _) v = HashMap 1 $ SA $ runST $ do
  arr <- SA.newSmallArray 8 Nothing
  SA.writeSmallArray arr ((8 + h1 `mod` 8) `mod` 8) (Just (Just (k, v)))
  SA.freezeSmallArray arr 0 8
{-# INLINABLE singH #-}

empty :: HashMap k v -> Bool
empty = (== 0) . size

data Resize = 
    ToHigh 
  | ToLow 
  | ToSize {-# UNPACK #-} !Int  deriving (Eq, Show)

changeSize = 256 :: Int
resizeHelper :: Resize -> Int -> Arr (Maybe (Maybe (k, v)))
resizeHelper ToHigh oldCap = let newCap = 2 * oldCap in
  if newCap > changeSize
    then A $ runST $ do
      arr <- A.newArray newCap Nothing
      A.unsafeFreezeArray arr
    else SA $ runST $ do
      arr <- SA.newSmallArray newCap Nothing
      SA.unsafeFreezeSmallArray arr
resizeHelper ToLow oldCap = let newCap = oldCap `div` 2 in
  if newCap > changeSize
    then A $ runST $ do
      arr <- A.newArray newCap Nothing
      A.unsafeFreezeArray arr
    else SA $ runST $ do
      arr <- SA.newSmallArray newCap Nothing
      SA.unsafeFreezeSmallArray arr
resizeHelper (ToSize s) _ = if s > changeSize
  then A $ runST $ do
    arr <- A.newArray s Nothing
    A.unsafeFreezeArray arr
  else SA $ runST $ do
    arr <- SA.newSmallArray s Nothing
    SA.unsafeFreezeSmallArray arr
{-# INLINABLE resizeHelper #-}

resize :: (DoubleHashable k) => Resize -> HashMap k v -> HashMap k v
resize to m@(HashMap size v) =
  if to == ToLow && length v == 8 then m else
  foldl (\m p -> case p of
            (Just (Just (k, v))) -> resizeUnsafeInsert k v m
            _                    -> m
        ) (HashMap 0 $ resizeHelper to $ length v) v
{-# INLINABLE resize #-}

insertHelper :: (Eq k) => Arr (Maybe (Maybe (k, v))) -> Int -> (k, v) -> Arr (Maybe (Maybe (k, v)))
insertHelper (A v) ind kv = A $ runST $ do
      v' <- A.unsafeThawArray v
      A.writeArray v' ind $ Just $ Just kv
      A.unsafeFreezeArray v'
insertHelper (SA v) ind kv = SA $ runST $ do
      v' <- SA.unsafeThawSmallArray v
      SA.writeSmallArray v' ind $ Just $ Just kv
      SA.unsafeFreezeSmallArray v'
{-# INLINABLE insertHelper #-}

resizeMetaInsert :: (Eq k, DoubleHashable k) => (HashMap k v -> Altered v -> (HC.Hashed k, v) -> HashMap k v) -> HC.Hashed k -> v -> HashMap k v -> HashMap k v
resizeMetaInsert _ keyHashed v Null = singH keyHashed v
resizeMetaInsert f keyHashed@(HC.Hashed k hash1' hash2') v m@(HashMap size v') = f m (lookupInsertV keyHashed v') (keyHashed, v)
{-# INLINE resizeMetaInsert #-}

resizeInsertWith :: (DoubleHashable k) => (v -> v -> v) -> HC.Hashed k -> v -> HashMap k v -> HashMap k v
resizeInsertWith f = resizeMetaInsert $ \m@(HashMap size v') ind kv@(k, v) ->
        let oldCap = length v'
         in case ind of
              ClusterEnd -> resize ToHigh $ resizeUnsafeInsert k v m
              Insert ind' ->
                if (size + 1) % oldCap >= maxLoadFactor
                  then resize ToHigh $ resizeUnsafeInsert k v m
                  else HashMap (size + 1) $ insertHelper v' ind' kv
              Update ind' oldV -> HashMap size $ insertHelper v' ind' (k, f v oldV)
{-# INLINABLE resizeInsertWith #-}

resizeUnsafeInsertWith :: (DoubleHashable k) => (v -> v -> v) -> HC.Hashed k -> v -> HashMap k v -> HashMap k v
resizeUnsafeInsertWith f = resizeMetaInsert $ \m@(HashMap size v') ind kv@(k@(HC.Hashed _ h1 h2), v) ->
        let oldCap = length v'
         in case ind of
              ClusterEnd -> error $ show oldCap ++ " " ++ show size ++ " " ++ show h1 ++ " " ++ show h2
              Insert ind' -> HashMap (size + 1) $ insertHelper v' ind' (k, v)
              Update ind' oldV -> HashMap size $ insertHelper v' ind' (k, f v oldV)
{-# INLINABLE resizeUnsafeInsertWith #-}

resizeInsert :: (DoubleHashable k) => HC.Hashed k -> v -> HashMap k v -> HashMap k v
resizeInsert = resizeInsertWith const
{-# INLINE resizeInsert #-}

resizeUnsafeInsert :: (DoubleHashable k) => HC.Hashed k -> v -> HashMap k v -> HashMap k v
resizeUnsafeInsert = resizeUnsafeInsertWith const
{-# INLINABLE resizeUnsafeInsert #-}

unsafeInsert :: (HasCallStack, DoubleHashable k) => k -> v -> HashMap k v -> HashMap k v
unsafeInsert = unsafeInsertWith const
{-# INLINE unsafeInsert #-}

unsafeInsertWith :: (DoubleHashable k) => (v -> v -> v) -> k -> v -> HashMap k v -> HashMap k v
unsafeInsertWith f = resizeUnsafeInsertWith f . HC.hashed
{-# INLINE unsafeInsertWith #-}

insert :: (DoubleHashable k) => k -> v -> HashMap k v -> HashMap k v
insert = insertWith const
{-# INLINABLE insert #-}

insertWith :: (DoubleHashable k) => (v -> v -> v) -> k -> v -> HashMap k v -> HashMap k v
insertWith f = resizeInsertWith f . HC.hashed
{-# INLINABLE insertWith #-}

lookupInsertV :: (DoubleHashable k) => HC.Hashed k -> Arr (Maybe (Maybe (HC.Hashed k, v))) -> Altered v
lookupInsertV keyHashed@(HC.Hashed _ hash1' hash2') v' = case indA v' h1 of
    Nothing -> Insert h1
    Just Nothing -> Insert h1 -- was deleted
    Just (Just (k', v'')) -> if keyHashed == k'
      then Update h1 v'' -- was setted
      else helper ((h1 + h2) `mod` len)
  where
    len = length v'
    h1 = (len + (hash1' `mod` len)) `mod` len
    h2 = (len + ((2 * hash2' + 1) `mod` len)) `mod` len
    helper curInd = if curInd == h1
      then ClusterEnd
      else case p of
        Nothing -> Insert curInd
        Just Nothing -> Insert curInd -- was deleted
        Just (Just (k', v'')) -> if keyHashed == k'
          then Update curInd v''
          else helper ((curInd + h2) `mod` len)
      where
        p = indA v' curInd
{-# INLINABLE lookupInsertV #-}

lookupV' :: (DoubleHashable k) => HC.Hashed k -> Arr (Maybe (Maybe (HC.Hashed k, v))) -> Altered v
lookupV' keyHashed@(HC.Hashed _ hash1' hash2') v' = case indA v' h1 of
    Nothing -> ClusterEnd
    Just Nothing -> helper ((h1 + h2) `mod` len) -- was deleted
    Just (Just (k', v'')) -> if keyHashed == k'
      then Update h1 v'' -- was setted
      else helper ((h1 + h2) `mod` len)
  where
    len = length v'
    h1 = (len + (hash1' `mod` len)) `mod` len
    h2 = (len + ((2 * hash2' + 1) `mod` len)) `mod` len
    helper curInd = if curInd == h1
      then ClusterEnd
      else case p of
        Nothing -> ClusterEnd
        Just Nothing -> helper ((curInd + h2) `mod` len) -- was deleted
        Just (Just (k', v'')) -> if keyHashed == k'
          then Update curInd v''
          else helper ((curInd + h2) `mod` len)
      where
        p = indA v' curInd
{-# INLINABLE lookupV' #-}

lookup :: (DoubleHashable k) => k -> HashMap k v -> Maybe v
lookup k Null = Nothing
lookup k (HashMap _ v) = case lookupV' (HC.hashed k) v of
    Insert _ -> Nothing
    Update _ v -> Just v
    ClusterEnd -> Nothing
{-# INLINE lookup #-}

data Altered v =
    Insert {-# UNPACK #-} !Int
  | Update {-# UNPACK #-} !Int v
  | ClusterEnd

alterF :: (Functor f, DoubleHashable k) => (Maybe v -> f (Maybe v)) -> k -> HashMap k v -> f (HashMap k v)
alterF f k Null = f Nothing <&> \case
    Nothing -> Null
    Just v -> singleton k v
alterF f k m@(HashMap size v) = let oldCap = length v in
  case lookupInsertV keyHashed v of
    Insert ind -> f Nothing <&> \case
        Nothing -> m
        Just val' -> if (size + 1) % oldCap >= maxLoadFactor
                  then resize ToHigh $ resizeUnsafeInsert keyHashed val' m
                  else HashMap (size + 1) $ insertHelper v ind (keyHashed, val')
    Update ind val' -> f (Just val') <&> \case
        Just val'' -> HashMap size $ insertHelper v ind (keyHashed, val'')
        Nothing -> let map = HashMap (size - 1) $ deleteHelper v ind
            in if (size - 1) % oldCap <= minLoadFactor
              then resize ToLow map
              else map
    ClusterEnd -> f Nothing <&> \case
        Nothing -> resize ToHigh m
        Just val' -> insert k val' $ resize ToHigh m
    where
      keyHashed = HC.hashed k

fromList :: (DoubleHashable k) => [(k, v)] -> HashMap k v
fromList [] = null
fromList [(k,v)] = singleton k v
fromList ((k,v) : l) = foldl' (\m (k, v) -> insert k v m) (singleton k v) l
 where
  len = length l

fromListWith :: DoubleHashable k => (v -> v -> v) -> [(k, v)] -> HashMap k v
fromListWith _ [] = null
fromListWith _ [(k,v)] = singleton k v
fromListWith f ((k,v) : l) = foldl' (\m (k, v) -> insertWith f k v m) (singleton k v) l
 where
  len = length l

metaDelete :: (DoubleHashable k) => (HashMap k v -> Maybe Int -> k -> HashMap k v) -> k -> HashMap k v -> HashMap k v
metaDelete f k Null = Null
metaDelete f k m@(HashMap size v') = f m ind k
 where
  len = length v'
  h1 = (len + hash1 k `mod` len) `mod` len
  h2 = (len + hash2 k `mod` len) `mod` len
  ind :: Maybe Int
  ind
    | isNothing p = Nothing -- wasn't setted
    | isNothing (fromJust p) = helper ((h1 + h2) `mod` len) -- was deleted
    | HC.unhashed (fst (fromJust . fromJust $ p)) == k = Just h1 -- was setted
    | otherwise = helper ((h1 + h2) `mod` len)
   where
    p = indA v' h1
  helper :: Int -> Maybe Int
  helper curInd
    | curInd == h1 = Nothing
    | isNothing p = Nothing
    | isNothing (fromJust p) = helper ((curInd + h2) `mod` len) -- was deleted
    | HC.unhashed (fst (fromJust . fromJust $ p)) == k = Just curInd
    | otherwise = helper ((curInd + h2) `mod` len)
   where
    p = indA v' curInd

deleteHelper :: Arr (Maybe (Maybe (k, v))) -> Int -> Arr (Maybe (Maybe (k, v)))
deleteHelper (A v) ind = A $ runST $ do
      v' <- A.unsafeThawArray v
      A.writeArray v' ind $ Just Nothing
      A.unsafeFreezeArray v'
deleteHelper (SA v) ind = SA $ runST $ do
      v' <- SA.unsafeThawSmallArray v
      SA.writeSmallArray v' ind $ Just Nothing
      SA.unsafeFreezeSmallArray v'

delete :: (DoubleHashable k) => k -> HashMap k v -> HashMap k v
delete = metaDelete $ \m@(HashMap size v') ind k ->
  let oldCap = length v' in
   case ind of
        Nothing -> m
        Just ind' ->
          let map = HashMap (size - 1) $ deleteHelper v' ind'
           in if (size - 1) % 1 <= oldCap % 1 * minLoadFactor then resize ToLow map else map

unsafeDelete :: (DoubleHashable k) => k -> HashMap k v -> HashMap k v
unsafeDelete = metaDelete $ \m@(HashMap size v') ind k ->
  let oldCap = length v' in
   case ind of
        Nothing -> m
        Just ind' -> HashMap (size - 1) $ deleteHelper v' ind'

{-
data IsDeleted k v = IsDeleted v (HashMap k v) | IsNotDeleted (HashMap k v)

deleteMaybe :: (DoubleHashable k) => k -> HashMap k v -> IsDeleted k v
deleteMaybe = metaDelete $ \m@(HashMap _ _ _ size v') ind k ->
  let oldCap = V.length v'
   in case ind of
        Nothing -> IsNotDeleted m
        Just ind' -> IsDeleted (val (v' V.! ind')) $
          let map = defH m (size - 1) $ v' V.// [(ind', Just Nothing)]
           in if (size - 1) % 1 <= (oldCap % 1) * minLoadFactor then resize ToLow map else map

unsafeDeleteMaybe :: (DoubleHashable k) => k -> HashMap k v -> IsDeleted k v
unsafeDeleteMaybe = metaDelete $ \m@(HashMap _ _ _ size v') ind k ->
  let oldCap = V.length v'
   in case ind of
        Nothing -> IsNotDeleted m
        Just ind' -> IsDeleted (val (v' V.! ind')) $ defH m (size - 1) $ v' V.// [(ind', Just Nothing)]
        -}