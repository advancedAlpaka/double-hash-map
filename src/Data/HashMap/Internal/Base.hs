{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.HashMap.Internal.Base where

import Control.Exception (throw)
import Data.Bits (Bits (..), FiniteBits (..))
import Data.Foldable hiding (null)
import Data.Hashable (Hashable (hash, hashWithSalt))
import Data.HashMap.Internal.Class (DoubleHashable (hash2, hash1))
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

data HashMap k v = Null | HashMap {
  size' :: !Int,
  buckets :: {-# UNPACK #-} !(Arr (Maybe (Maybe (k, v)))) }

size :: HashMap k v -> Int
size Null = 0
size h = size' h

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
null@(HashMap _ (SA nullArray)) = HashMap 0 $ SA $ runST $ do
  arr <- SA.newSmallArray 8 Nothing
  SA.freezeSmallArray arr 0 8

singleton :: (DoubleHashable k) => k -> v -> HashMap k v
singleton k v = HashMap 1 $ SA $ runST $ do
  arr <- SA.newSmallArray 8 Nothing
  SA.writeSmallArray arr (hash k `mod` 8) (Just (Just (k, v)))
  SA.freezeSmallArray arr 0 8

empty :: HashMap k v -> Bool
empty = (== 0) . size

data Resize = ToHigh | ToLow deriving (Eq, Show)

changeSize = 256 :: Int
resizeHelper :: Resize -> Arr (Maybe (Maybe (k, v))) -> Arr (Maybe (Maybe (k, v)))
resizeHelper ToHigh (A v) = A $ runST $ do
                    let newCap = A.sizeofArray v * 2
                    arr <- A.newArray newCap Nothing
                    A.freezeArray arr 0 newCap
resizeHelper ToLow sa@(SA v) = SA $ runST $ do
                    let newCap = SA.sizeofSmallArray v `div` 2
                    arr <- SA.newSmallArray newCap Nothing
                    SA.freezeSmallArray arr 0 newCap
resizeHelper ToHigh (SA v) = let newCap = 2 * SA.sizeofSmallArray v in
  if newCap > changeSize
    then A $ runST $ do
      arr <- A.newArray newCap Nothing
      A.freezeArray arr 0 newCap
    else SA $ runST $ do
      arr <- SA.newSmallArray newCap Nothing
      SA.freezeSmallArray arr 0 newCap
resizeHelper ToLow (A v) = let newCap = A.sizeofArray v `div` 2 in
  if newCap > changeSize
    then A $ runST $ do
      arr <- A.newArray newCap Nothing
      A.freezeArray arr 0 newCap
    else SA $ runST $ do
      arr <- SA.newSmallArray newCap Nothing
      SA.freezeSmallArray arr 0 newCap
{-# INLINABLE resizeHelper #-}

resize :: (DoubleHashable k) => Resize -> HashMap k v -> HashMap k v
resize to m@(HashMap size v) =
  if to == ToLow && (length v == 8) then m else
  foldl (\m p -> case p of
            (Just (Just (k, v))) -> unsafeInsert k v m
            _                    -> m
        ) (HashMap 0 $ resizeHelper to v) v
{-# INLINABLE resize #-}
--meta :: (HasCallStack, DoubleHashable k) => (HashMap k v -> Maybe a -> (k, v) -> HashMap k v) -> k -> v -> HashMap k v -> HashMap k v
--meta = _

metaInsert :: (HasCallStack, DoubleHashable k) =>
  (HashMap k v -> Maybe (Either Int Int) -> (k, v) -> HashMap k v)
  -> k -> v -> HashMap k v -> HashMap k v
metaInsert f k v m@(HashMap size v') = f m ind (k, v)
 where
  len = length v'
  h1 = hash1 k `mod` len
  h2 = hash2 k `mod` len
  ind :: Maybe (Either Int Int)
  ind
    | isNothing p = Just $ Left h1 -- wasn't setted
    | isNothing (fromJust p) = Just $ Left h1 -- was deleted
    | fst (fromJust . fromJust $ p) == k = Just $ Right h1 -- was setted
    | otherwise = helper ((h1 + h2) `mod` len)
   where
    p = indA v' h1
  helper :: Int -> Maybe (Either Int Int)
  helper curInd
    | curInd == h1 = Nothing
    | isNothing p = Just $ Left curInd
    | isNothing (fromJust p) = Just $ Left curInd -- was deleted
    | fst (fromJust . fromJust $ p) == k = Just $ Right curInd
    | otherwise = helper ((curInd + h2) `mod` len)
   where
    p = indA v' curInd
{-# INLINABLE metaInsert #-}

insertHelper :: (Eq k) => Arr (Maybe (Maybe (k, v))) -> Int -> (k, v) -> Arr (Maybe (Maybe (k, v)))
insertHelper (A v) ind kv = A $ runST $ do
      v' <- A.unsafeThawArray v
      A.writeArray v' ind $ Just $ Just kv
      A.unsafeFreezeArray v'
insertHelper (SA v) ind kv = SA $ runST $ 
  if SA.sizeofSmallArray v == 8 
    then do
      v' <- SA.thawSmallArray v 0 8
      SA.writeSmallArray v' ind $ Just $ Just kv
      SA.unsafeFreezeSmallArray v'
    else do
      v' <- SA.unsafeThawSmallArray v
      SA.writeSmallArray v' ind $ Just $ Just kv
      SA.unsafeFreezeSmallArray v'
{-# INLINABLE insertHelper #-}

unsafeInsert :: (HasCallStack, DoubleHashable k) => k -> v -> HashMap k v -> HashMap k v
unsafeInsert = unsafeInsertWith const
{-# INLINABLE unsafeInsert #-}

unsafeInsertWith :: (DoubleHashable k) => (v -> v -> v) -> k -> v -> HashMap k v -> HashMap k v
unsafeInsertWith f =
  metaInsert
    ( \m@(HashMap size v') ind kv@(k, v) ->
        let oldCap = length v'
         in case ind of
              Nothing -> undefined--error $ "unsafeSet: Nothing \n oldCap : " ++ show oldCap ++ "\n size : " ++ show size ++ "\n hash1 k : " ++ show (hash1 k `mod` oldCap) ++ "\n hash2 k : " ++ show (hash2 k `mod` oldCap)
              Just (Left ind') -> HashMap (size + 1) $ insertHelper v' ind' kv
              Just (Right ind') -> HashMap size $ insertHelper v' ind' (k, f v (val $ indA v' ind'))
    )
{-# INLINABLE unsafeInsertWith #-}

insert :: (DoubleHashable k) => k -> v -> HashMap k v -> HashMap k v
insert = insertWith const
{-# INLINABLE insert #-}

insertWith :: (DoubleHashable k) => (v -> v -> v) -> k -> v -> HashMap k v -> HashMap k v
insertWith f =
  metaInsert
    ( \m@(HashMap size v') ind kv@(k, v) ->
        let oldCap = length v'
         in case ind of
              Nothing -> insert k v $ resize ToHigh m
              Just (Left ind') ->
                if (size + 1) % 1 >= (oldCap % 1) * maxLoadFactor
                  then unsafeInsert k v $ resize ToHigh m
                  else HashMap (size + 1) $ insertHelper v' ind' kv
              Just (Right ind') -> HashMap size $ insertHelper v' ind' (k, f v (val $ indA v' ind'))
    )
{-# INLINABLE insertWith #-}

lookup :: (DoubleHashable k) => k -> HashMap k v -> Maybe v
lookup k (HashMap _ v) =
  case indA v h1 of
    Nothing              -> Nothing -- wasn't setted
    Just Nothing         -> helper ((h1 + h2) `mod` len)
    Just (Just (k', v')) -> if k == k' then Just v' else helper ((h1 + h2) `mod` len)
 where
  len = length v
  h1 = (len + (hash1 k `mod` len)) `mod` len
  h2 = (len + (hash2 k `mod` len)) `mod` len
  helper ind =
    if ind == h1
      then Nothing
      else
        ( case indA v ind of
            Nothing -> Nothing -- wasn't setted
            Just Nothing -> helper ((h1 + h2) `mod` len)
            Just (Just (k', v'')) -> if k == k' then Just v'' else helper ((ind + h2) `mod` len)
        )

fromList :: (DoubleHashable k) => [(k, v)] -> HashMap k v
fromList [] = null
fromList ((k,v) : l) = foldl' (\m (k, v) -> insert k v m) (singleton k v) l
 where
  len = length l

metaDelete :: (DoubleHashable k) => (HashMap k v -> Maybe Int -> k -> r) -> k -> HashMap k v -> r
metaDelete f k m@(HashMap size v') = f m ind k
 where
  len = length v'
  h1 = (len + (hash1 k `mod` len)) `mod` len
  h2 = (len + (hash2 k `mod` len)) `mod` len
  ind :: Maybe Int
  ind
    | isNothing p = Nothing -- wasn't setted
    | isNothing (fromJust p) = helper ((h1 + h2) `mod` len) -- was deleted
    | fst (fromJust . fromJust $ p) == k = Just h1 -- was setted
    | otherwise = helper ((h1 + h2) `mod` len)
   where
    p = indA v' h1
  helper :: Int -> Maybe Int
  helper curInd
    | curInd == h1 = Nothing
    | isNothing p = Nothing
    | isNothing (fromJust p) = helper ((curInd + h2) `mod` len) -- was deleted
    | fst (fromJust . fromJust $ p) == k = Just curInd
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
           in if (size - 1) % 1 <= (oldCap % 1) * minLoadFactor then resize ToLow map else map

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