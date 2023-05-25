
module Data.HashMap.Internal.Base where

import Control.Exception (throw)
import Data.HashMap.Internal.Class (DoubleHashable (hash2))
import Data.Hashable (Hashable (hash, hashWithSalt))
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Ratio (Ratio, (%))
import qualified Data.Vector as V
import GHC.Stack (HasCallStack)
import Data.Foldable (Foldable(..))
import Data.Bits (FiniteBits(..), Bits (..))

data HashMap k v = HashMap Int (V.Vector (Maybe (Maybe (k, v))))

-- Nothing - wasn't setted
-- Just Nothing - was deleted
-- Just (Just (k, v)) - was setted

loadFactor :: Ratio Int
loadFactor = 8 % 10

key :: Maybe (Maybe (c, b)) -> c
key = fst . fromJust . fromJust
val = snd . fromJust . fromJust

null :: HashMap k v
null = HashMap 0 $ V.replicate 8 Nothing

singleton :: (DoubleHashable k) => k -> v -> HashMap k v
singleton k v = HashMap 1 $ V.replicate 8 Nothing V.// [(hash k `mod` 8, Just (Just (k, v)))]

size :: HashMap k v -> Int
size (HashMap s _) = s

resize :: (DoubleHashable k) => Int -> HashMap k v -> HashMap k v
resize newCapacity (HashMap size v) =
    V.foldl
        ( \m p -> case p of
            (Just (Just (k, v))) -> unsafeInsert k v m
            _ -> m
        )
        (HashMap 0 $ V.replicate newCapacity Nothing)
        v

metaInsert :: (HasCallStack, DoubleHashable k) => (HashMap k v -> Maybe (Either Int Int) -> (k, v) -> HashMap k v) -> k -> v -> HashMap k v -> HashMap k v
metaInsert f k v m@(HashMap size v') = f m ind (k, v)
  where
    oldCap = V.length v'
    h1 = hash k `mod` V.length v'
    h2 = hash2 k `mod` V.length v'
    ind :: Maybe (Either Int Int)
    ind | isNothing p = Just $ Left h1 -- wasn't setted
        | isNothing (fromJust p) = Just $ Left h1 -- was deleted
        | fst (fromJust . fromJust $ p) == k = Just $ Right h1 -- was setted
        | otherwise = helper ((h1 + h2) `mod` V.length v')
      where
        p = v' V.! h1
    helper :: Int -> Maybe (Either Int Int)
    helper curInd
        | curInd == h1 = Nothing
        | isNothing p = Just $ Left curInd
        | isNothing (fromJust p) = Just $ Left curInd -- was deleted
        | fst (fromJust . fromJust $ p) == k = Just $ Right curInd
        | otherwise = helper ((curInd + h2) `mod` V.length v')
      where
        p = v' V.! curInd

unsafeInsert :: (HasCallStack, DoubleHashable k) => k -> v -> HashMap k v -> HashMap k v
unsafeInsert =
    metaInsert
        ( \(HashMap size v') ind kv -> case ind of
            Nothing -> error "unsafeSet: Nothing"
            Just (Left ind') -> HashMap (size + 1) $ v' V.// [(ind', Just $ Just kv)]
            Just (Right ind') -> HashMap size $ v' V.// [(ind', Just $ Just kv)]
        )

insert :: (DoubleHashable k, Eq k) => k -> v -> HashMap k v -> HashMap k v
insert =
    metaInsert
        ( \m@(HashMap size v') ind kv@(k, v) ->
            let oldCap = V.length v'
             in case ind of
                    Nothing -> insert k v $ resize (2 * oldCap) m
                    Just (Left ind') ->
                        let map = HashMap (size + 1) $ v' V.// [(ind', Just $ Just kv)]
                         in if (size + 1) % 1 >= (oldCap % 1) * loadFactor then resize (2 * oldCap) map else map
                    Just (Right ind') -> HashMap size $ v' V.// [(ind', Just $ Just kv)]
        )

insertWith :: (DoubleHashable k) => (v -> v -> v) -> k -> v -> HashMap k v -> HashMap k v
insertWith f =
    metaInsert
        ( \m@(HashMap size v') ind kv@(k, v) ->
            let oldCap = V.length v'
             in case ind of
                    Nothing -> insert k v $ resize (2 * oldCap) m
                    Just (Left ind') ->
                        let map = HashMap (size + 1) $ v' V.// [(ind', Just $ Just kv)]
                         in if (size + 1) % 1 >= (oldCap % 1) * loadFactor then resize (2 * oldCap) map else map
                    Just (Right ind') -> HashMap size $ v' V.// [(ind', Just $ Just (k, f v (val (v' V.! ind'))))]
        )

lookup :: (DoubleHashable k) => k -> HashMap k v -> Maybe v
lookup k (HashMap _ v) = case v V.! h1 of
    Nothing -> Nothing -- wasn't setted
    Just Nothing -> helper ((h1 + h2) `mod` V.length v)
    Just (Just (k', v')) -> if k == k' then Just v' else helper ((h1 + h2) `mod` V.length v)
  where
    h1 = hash k `mod` V.length v
    h2 = hash2 k `mod` V.length v
    helper ind =
        if ind == h1
            then Nothing
            else
                ( case v V.! ind of
                    Nothing -> Nothing -- wasn't setted
                    Just Nothing -> helper ((h1 + h2) `mod` V.length v)
                    Just (Just (k', v'')) -> if k == k' then Just v'' else helper ((ind + h2) `mod` V.length v)
                )

fromList :: (DoubleHashable k) => [(k, v)] -> HashMap k v
fromList l = foldl' (\m (k, v) -> unsafeInsert k v m) (HashMap 0 $ V.replicate (bit log2pl1) Nothing) l
    where
        len = length l
        log2pl1 = finiteBitSize len - countLeadingZeros len

delete :: (DoubleHashable k) => k -> HashMap k v -> HashMap k v
delete k m@(HashMap size v') =
    let oldCap = V.length v'
     in case ind of
            Nothing -> m
            Just ind' ->
                let map = HashMap (size - 1) $ v' V.// [(ind', Just Nothing)]
                 in if (size - 1) % 1 <= (oldCap % 1) * (1 - loadFactor) then resize (oldCap `div` 2) map else map
  where
    oldCap = V.length v'
    h1 = hash k `mod` V.length v'
    h2 = hash2 k `mod` V.length v'
    ind :: Maybe Int
    ind | isNothing p = Nothing -- wasn't setted
        | isNothing (fromJust p) = helper ((h1 + h2) `mod` V.length v') -- was deleted
        | fst (fromJust . fromJust $ p) == k = Just h1 -- was setted
        | otherwise = helper ((h1 + h2) `mod` V.length v')
      where
        p = v' V.! h1
    helper :: Int -> Maybe Int
    helper curInd
        | curInd == h1 = Nothing
        | isNothing p = Nothing
        | isNothing (fromJust p) = helper ((curInd + h2) `mod` V.length v') -- was deleted
        | fst (fromJust . fromJust $ p) == k = Just curInd
        | otherwise = helper ((curInd + h2) `mod` V.length v')
      where
        p = v' V.! curInd