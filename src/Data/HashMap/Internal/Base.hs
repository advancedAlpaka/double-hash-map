module Data.HashMap.Internal.Base where

import Control.Exception (throw)
import Data.Bits (Bits (..), FiniteBits (..))
import Data.Foldable (Foldable (..))
import Data.Hashable (Hashable (hash, hashWithSalt))
import Data.HashMap.Internal.Class (DoubleHashable (hash2))
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Ratio (Ratio, (%))
import qualified Data.Vector as V
import GHC.Stack (HasCallStack)

data HashMap k v = HashMap [Int] [Int] [Int] Int (V.Vector (Maybe (Maybe (k, v))))

primes =
  2
    : minus
      [3 ..]
      ( foldr
          (\p r -> (p * p) : union [p * p + p, p * p + 2 * p ..] r)
          []
          primes
      )

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

loadFactor :: Ratio Int
loadFactor = 8 % 10

key = fst . fromJust . fromJust
val = snd . fromJust . fromJust

null :: HashMap k v
null = def 0 $ V.replicate 7 Nothing

singleton :: (DoubleHashable k) => k -> v -> HashMap k v
singleton k v = def 1 $ V.replicate 7 Nothing V.// [(hash k `mod` 7, Just (Just (k, v)))]

size :: HashMap k v -> Int
size (HashMap _ _ _ s _) = s

resize :: (DoubleHashable k) => Bool -> HashMap k v -> HashMap k v
resize toHigh m@(HashMap lePr grPr nxtPr size v) =
  if not toHigh && Prelude.null lePr
    then m
    else
      V.foldl
        ( \m p -> case p of
            (Just (Just (k, v))) -> unsafeInsert k v m
            _                    -> m
        )
        ( if toHigh
            then
              ( case grPr of
                  [] ->
                    let (newSz, newPr) = findCap nxtPr
                     in HashMap (size : lePr) grPr newPr newSz v
                  (x : xs) -> HashMap (size : lePr) xs nxtPr x v
              )
            else
              ( case lePr of
                  []       -> undefined
                  (x : xs) -> HashMap xs (size : grPr) nxtPr x v
              )
        )
        v
 where
  findCap :: [Int] -> (Int, [Int])
  findCap (x : xs) = if x >= 2 * size then (x, xs) else findCap xs

metaInsert :: (HasCallStack, DoubleHashable k) => (HashMap k v -> Maybe (Either Int Int) -> (k, v) -> HashMap k v) -> k -> v -> HashMap k v -> HashMap k v
metaInsert f k v m@(HashMap _ _ _ size v') = f m ind (k, v)
 where
  oldCap = V.length v'
  h1 = hash k `mod` V.length v'
  h2 = hash2 k `mod` V.length v'
  ind :: Maybe (Either Int Int)
  ind
    | isNothing p = Just $ Left h1 -- wasn't setted
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
    ( \h@(HashMap _ _ _ size v') ind kv -> case ind of
        Nothing           -> error "unsafeSet: Nothing"
        Just (Left ind')  -> defH h (size + 1) $ v' V.// [(ind', Just $ Just kv)]
        Just (Right ind') -> defH h size $ v' V.// [(ind', Just $ Just kv)]
    )

insert :: (DoubleHashable k, Eq k) => k -> v -> HashMap k v -> HashMap k v
insert =
  metaInsert
    ( \m@(HashMap _ _ _ size v') ind kv@(k, v) ->
        let oldCap = V.length v'
         in case ind of
              Nothing -> insert k v $ resize True m
              Just (Left ind') ->
                let map = defH m (size + 1) $ v' V.// [(ind', Just $ Just kv)]
                 in if (size + 1) % 1 >= (oldCap % 1) * loadFactor then resize True map else map
              Just (Right ind') -> defH m size $ v' V.// [(ind', Just $ Just kv)]
    )

insertWith :: (DoubleHashable k) => (v -> v -> v) -> k -> v -> HashMap k v -> HashMap k v
insertWith f =
  metaInsert
    ( \m@(HashMap _ _ _ size v') ind kv@(k, v) ->
        let oldCap = V.length v'
         in case ind of
              Nothing -> insert k v $ resize True m
              Just (Left ind') ->
                let map = defH m (size + 1) $ v' V.// [(ind', Just $ Just kv)]
                 in if (size + 1) % 1 >= (oldCap % 1) * loadFactor then resize True map else map
              Just (Right ind') -> defH m size $ v' V.// [(ind', Just $ Just (k, f v (val (v' V.! ind'))))]
    )

lookup :: (DoubleHashable k) => k -> HashMap k v -> Maybe v
lookup k (HashMap _ _ _ _ v) = case v V.! h1 of
  Nothing              -> Nothing -- wasn't setted
  Just Nothing         -> helper ((h1 + h2) `mod` V.length v)
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
fromList l = foldl' (\m (k, v) -> unsafeInsert k v m) (HashMap lePr [] nxtPr 0 $ V.replicate reqSize Nothing) l
 where
  len = length l
  (reqSize, nxtPr, lePr) = findCap 7 [] primes
  findCap :: Int -> [Int] -> [Int] -> (Int, [Int], [Int])
  findCap old oldPr (x : xs)
    | x >= 2 * len && x >= 2 * old = (x, xs, old : oldPr)
    | x >= 2 * old = findCap x (old : oldPr) xs
    | otherwise = findCap old oldPr xs

delete :: (DoubleHashable k) => k -> HashMap k v -> HashMap k v
delete k m@(HashMap _ _ _ size v') =
  let oldCap = V.length v'
   in case ind of
        Nothing -> m
        Just ind' ->
          let map = defH m (size - 1) $ v' V.// [(ind', Just Nothing)]
           in if (size - 1) % 1 <= (oldCap % 1) * (1 - loadFactor) then resize False map else map
 where
  oldCap = V.length v'
  h1 = hash k `mod` V.length v'
  h2 = hash2 k `mod` V.length v'
  ind :: Maybe Int
  ind
    | isNothing p = Nothing -- wasn't setted
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
