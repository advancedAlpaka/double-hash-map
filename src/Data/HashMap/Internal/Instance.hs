{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE MonoLocalBinds #-}

module Data.HashMap.Internal.Instance where

import Control.DeepSeq (NFData (..))
import Data.Bifunctor (Bifunctor (..))
import Data.HashMap.Internal.Base (HashMap (..), insert, size, fromList, buckets)
import Data.HashMap.Internal.Basic (union, map, foldrWithKey, toList, foldrWithKey')
import qualified Data.HashMap.Internal.Base as HB (null, lookup)
import Data.HashMap.Internal.Class (DoubleHashable)
import Data.List (intercalate)
import qualified Data.Primitive.Array as A
import Data.Foldable (Foldable(..))
import Data.Maybe (mapMaybe)
import Data.Bifoldable (Bifoldable (bifoldr))
import Text.Read (Read(readPrec, readListPrec), Lexeme (Ident), parens, prec, lexP)
import Data.Hashable (Hashable (hashWithSalt))

instance (Show k, Show v) => Show (HashMap k v) where
  --showsPrec :: HashMap k v -> String
  showsPrec d m = showParen (d > 10) $
    showString "fromList " . shows (Data.HashMap.Internal.Basic.toList m)

instance (Read k, Read e, DoubleHashable k) => Read (HashMap k e) where
  readPrec = parens $ prec 10 $ do
    Ident "fromList" <- lexP
    fromList <$> readPrec

fun f = \case
          Just (Just (_, v)) -> f v
          _ -> id
fun1 :: (b -> a -> b) -> b -> Maybe (Maybe (k, a)) -> b
fun1 f b = \case
          Just (Just (_, v)) ->  f b v
          _ -> b

instance Foldable (HashMap k) where
  foldr :: (a -> b -> b) -> b -> HashMap k a -> b
  foldr f z = foldr (fun f) z . buckets
  foldr' :: (a -> b -> b) -> b -> HashMap k a -> b
  foldr' f z = foldr' (fun f) z . buckets
  foldl :: (b -> a -> b) -> b -> HashMap k a -> b
  foldl f z = foldl (fun1 f) z . buckets
  foldl' :: (b -> a -> b) -> b -> HashMap k a -> b
  foldl' f z = foldl' (fun1 f) z . buckets
  length = size

instance Functor (HashMap k) where
  fmap :: (a -> b) -> HashMap k a -> HashMap k b
  fmap = Data.HashMap.Internal.Basic.map

instance (DoubleHashable k, Eq v) => Eq (HashMap k v) where
  (==) :: HashMap k v -> HashMap k v -> Bool
  (==) h1 h2 = size h1 == size h2 && equalHelper h1 h2
    where 
      equalHelper :: (DoubleHashable k, Eq v) => HashMap k v -> HashMap k v -> Bool
      equalHelper h1 h2 = 
            if size h1 < size h2 
              then equalHelper h2 h1 
              else foldrWithKey' (\k v acc -> acc && case HB.lookup k h2 of
                Just v' -> v == v'
                _ -> False) True h1

{-instance (DoubleHashable k, Eq v) => Ord (HashMap k v) where
  compare :: DoubleHashable k => HashMap k v -> HashMap k v -> Ordering
  compare = _-}

instance (NFData k, NFData v) => NFData (HashMap k v) where
  rnf Null = ()
  rnf (HashMap size v) = rnf size `seq` rnf v

{-instance (Hashable k, Eq v) => Hashable (HashMap k v) where
  hashWithSalt :: Int -> HashMap k v -> Int
  hashWithSalt salt (HashMap _ v) = hashWithSalt salt v-}

instance DoubleHashable k => Semigroup (HashMap k v) where
  (<>) :: HashMap k v -> HashMap k v -> HashMap k v
  (<>) = union

instance DoubleHashable k => Monoid (HashMap k v) where
  mempty :: HashMap k v
  mempty = HB.null