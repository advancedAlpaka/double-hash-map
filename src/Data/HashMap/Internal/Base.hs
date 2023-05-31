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
import GHC.Stack (HasCallStack)
import Control.Monad.ST (runST, ST)
import qualified Prelude
import Prelude hiding (null)

data HashMap k v =  HashMap {
  lessPr :: [Int],
  morePr :: [Int],
  nextPr ::[Int],
  size :: Int,
  buckets :: {-# UNPACK #-} !(A.Array (Maybe (Maybe (k, v)))) }

primes =
  2 : minus
      [3 ..]
      ( foldr
          (\p r -> (p * p) : union [p * p + p, p * p + 2 * p ..] r)
          []
          primes
      )
      where
        minus (x : xs) (y : ys) = case compare x y of
          LT -> x : minus xs (y : ys)
          EQ -> minus xs ys
          GT -> minus (x : xs) ys
        minus xs _ = xs

        union (x : xs) (y : ys) = case compare x y of
          LT -> x : union xs (y : ys)
          EQ -> x : union xs ys
          GT -> y : union (x : xs) ys

def = HashMap [] [] primes

defH (HashMap l g n _ _) = HashMap l g n

-- Nothing - wasn't setted
-- Just Nothing - was deleted
-- Just (Just (k, v)) - was setted

maxLoadFactor :: Ratio Int
maxLoadFactor = 3 % 4

minLoadFactor :: Ratio Int
minLoadFactor = 3 % 5

key = fst . fromJust . fromJust

val = snd . fromJust . fromJust

null :: HashMap k v
nullArray :: A.Array (Maybe (Maybe (k, v)))
null@(HashMap _ _ _ _ nullArray) = def 0 $ runST $ do
  arr <- A.newArray 7 Nothing
  A.freezeArray arr 0 7

singleton :: (DoubleHashable k) => k -> v -> HashMap k v
singleton k v = def 1 $ runST $ do
  arr <- A.newArray 7 Nothing
  A.writeArray arr (hash k `mod` 7) (Just (Just (k, v)))
  A.freezeArray arr 0 7

empty :: HashMap k v -> Bool
empty = (== 0) . size

data Rezize = ToHigh | ToLow

resize :: (DoubleHashable k) => Rezize -> HashMap k v -> HashMap k v
resize ToLow m@(HashMap lePr grPr nxtPr size v) =
  if Prelude.null lePr
    then m
    else
  foldl (\m p -> case p of
            (Just (Just (k, v))) -> unsafeInsert k v m
            _                    -> m
        ) ( case lePr of
                  []       -> undefined
                  (x : xs) -> HashMap xs (A.sizeofArray v : grPr) nxtPr 0 $ runST $ do
                    arr <- A.newArray x Nothing
                    A.freezeArray arr 0 x
              )
         v
resize ToHigh m@(HashMap lePr grPr nxtPr size v) =
      foldl
        ( \m p -> case p of
            (Just (Just (k, v))) -> unsafeInsert k v m
            _                    -> m
        ) (case grPr of
                  [] ->
                    let (newCap, newPr) = findCap nxtPr
                     in HashMap (A.sizeofArray v : lePr) grPr newPr 0 $ runST $ do
                        arr <- A.newArray newCap Nothing
                        A.freezeArray arr 0 newCap
                  (x : xs) -> HashMap (A.sizeofArray v : lePr) xs nxtPr 0 $ runST $ do
                    arr <- A.newArray x Nothing
                    A.freezeArray arr 0 x
              ) v
 where
  findCap :: [Int] -> (Int, [Int])
  findCap (x : xs@(x2 : _)) = if (x % 1) * maxLoadFactor >= (size % 1) && (x2 % 1) * minLoadFactor >= (size % 1)
--                                 || (x % 1) * maxLoadFactor < (size % 1) && (x2 % 1) * minLoadFactor < (size % 1) 
                                 then (x, xs) else findCap xs

metaInsert :: (HasCallStack, DoubleHashable k) =>
  (HashMap k v -> Maybe (Either Int Int) -> (k, v) -> HashMap k v)
  -> k -> v -> HashMap k v -> HashMap k v
metaInsert f k v m@(HashMap _ _ _ size v') = f m ind (k, v)
 where
  len = A.sizeofArray v'
  h1 = hash1 len k `mod` len
  h2 = hash2 len k `mod` len
  ind :: Maybe (Either Int Int)
  ind
    | isNothing p = Just $ Left h1 -- wasn't setted
    | isNothing (fromJust p) = Just $ Left h1 -- was deleted
    | fst (fromJust . fromJust $ p) == k = Just $ Right h1 -- was setted
    | otherwise = helper ((h1 + h2) `mod` len)
   where
    p = A.indexArray v' h1
  helper :: Int -> Maybe (Either Int Int)
  helper curInd
    | curInd == h1 = Nothing
    | isNothing p = Just $ Left curInd
    | isNothing (fromJust p) = Just $ Left curInd -- was deleted
    | fst (fromJust . fromJust $ p) == k = Just $ Right curInd
    | otherwise = helper ((curInd + h2) `mod` len)
   where
    p = A.indexArray v' curInd


insertHelper :: A.Array (Maybe (Maybe (k, v))) -> Int -> (k, v) -> A.Array (Maybe (Maybe (k, v)))
insertHelper v ind kv = runST $ do
      v' <- A.unsafeThawArray v
      vNull <- A.unsafeThawArray nullArray
      v <- if A.sameMutableArray v' vNull then
         A.cloneMutableArray vNull 0 7
        else return v'
      A.writeArray v ind $ Just $ Just kv
      A.unsafeFreezeArray v


unsafeInsert :: (HasCallStack, DoubleHashable k) => k -> v -> HashMap k v -> HashMap k v
unsafeInsert = unsafeInsertWith const

unsafeInsertWith :: (DoubleHashable k) => (v -> v -> v) -> k -> v -> HashMap k v -> HashMap k v
unsafeInsertWith f =
  metaInsert
    ( \m@(HashMap _ _ _ size v') ind kv@(k, v) ->
        let oldCap = A.sizeofArray v'
         in case ind of
              Nothing -> error $ "unsafeSet: Nothing \n oldCap : " ++ show oldCap ++ "\n size : " ++ show size ++ "\n hash1 k : " ++ show (hash1 oldCap k `mod` oldCap) ++ "\n hash2 k : " ++ show (hash2 oldCap k `mod` oldCap)
              Just (Left ind') -> defH m (size + 1) $ insertHelper v' ind' kv
              Just (Right ind') -> defH m size $ insertHelper v' ind' (k, f v (val $ A.indexArray v' ind'))
    )


insert :: (DoubleHashable k) => k -> v -> HashMap k v -> HashMap k v
insert = insertWith const

insertWith :: (DoubleHashable k) => (v -> v -> v) -> k -> v -> HashMap k v -> HashMap k v
insertWith f =
  metaInsert
    ( \m@(HashMap _ _ _ size v') ind kv@(k, v) ->
        let oldCap = A.sizeofArray v'
         in case ind of
              Nothing -> insert k v $ resize ToHigh m
              Just (Left ind') ->
                let map = defH m (size + 1) $ insertHelper v' ind' kv
                 in if (size + 1) % 1 >= (oldCap % 1) * maxLoadFactor then resize ToHigh map else map
              Just (Right ind') -> defH m size $ insertHelper v' ind' (k, f v (val $ A.indexArray v' ind'))
    )


lookup :: (DoubleHashable k) => k -> HashMap k v -> Maybe v
lookup k (HashMap _ _ _ _ v) =
  case A.indexArray v h1 of
    Nothing              -> Nothing -- wasn't setted
    Just Nothing         -> helper ((h1 + h2) `mod` len)
    Just (Just (k', v')) -> if k == k' then Just v' else helper ((h1 + h2) `mod` len)
 where
  len = A.sizeofArray v
  h1 = hash1 len k `mod` len
  h2 = hash2 len k `mod` len
  helper ind =
    if ind == h1
      then Nothing
      else
        ( case A.indexArray v ind of
            Nothing -> Nothing -- wasn't setted
            Just Nothing -> helper ((h1 + h2) `mod` len)
            Just (Just (k', v'')) -> if k == k' then Just v'' else helper ((ind + h2) `mod` len)
        )

fromList :: (DoubleHashable k) => [(k, v)] -> HashMap k v
fromList [] = null
fromList ((k,v) : l) = foldl' (\m (k, v) -> insert k v m) (singleton k v) l
{- where
  len = length l
  (reqSize, nxtPr, lePr) = findCap 7 [] primes
  findCap :: Int -> [Int] -> [Int] -> (Int, [Int], [Int])
  findCap old oldPr (x : xs)
    | x >= 2 * len && x >= 2 * old = (x, xs, old : oldPr)
    | x >= 2 * old = findCap x (old : oldPr) xs
    | otherwise = findCap old oldPr xs
-}

metaDelete :: (DoubleHashable k) => (HashMap k v -> Maybe Int -> k -> r) -> k -> HashMap k v -> r
metaDelete f k m@(HashMap _ _ _ size v') = f m ind k
 where
  len = A.sizeofArray v'
  h1 = hash1 len k `mod` len
  h2 = hash2 len k `mod` len
  ind :: Maybe Int
  ind
    | isNothing p = Nothing -- wasn't setted
    | isNothing (fromJust p) = helper ((h1 + h2) `mod` len) -- was deleted
    | fst (fromJust . fromJust $ p) == k = Just h1 -- was setted
    | otherwise = helper ((h1 + h2) `mod` len)
   where
    p = A.indexArray v' h1
  helper :: Int -> Maybe Int
  helper curInd
    | curInd == h1 = Nothing
    | isNothing p = Nothing
    | isNothing (fromJust p) = helper ((curInd + h2) `mod` len) -- was deleted
    | fst (fromJust . fromJust $ p) == k = Just curInd
    | otherwise = helper ((curInd + h2) `mod` len)
   where
    p = A.indexArray v' curInd

delete :: (DoubleHashable k) => k -> HashMap k v -> HashMap k v
delete = metaDelete $ \m@(HashMap _ _ _ size v') ind k ->
  let oldCap = A.sizeofArray v' in
   case ind of
        Nothing -> m
        Just ind' ->
          let map = defH m (size - 1) $ runST $ do
                arr <- A.unsafeThawArray v'
                A.writeArray arr ind' (Just Nothing)
                A.unsafeFreezeArray arr
           in if (size - 1) % 1 <= (oldCap % 1) * minLoadFactor then resize ToLow map else map

unsafeDelete :: (DoubleHashable k) => k -> HashMap k v -> HashMap k v
unsafeDelete = metaDelete $ \m@(HashMap _ _ _ size v') ind k ->
  let oldCap = A.sizeofArray v' in
   case ind of
        Nothing -> m
        Just ind' -> defH m (size - 1) $ runST $ do
                arr <- A.unsafeThawArray v'
                A.writeArray arr ind' (Just Nothing)
                A.unsafeFreezeArray arr

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