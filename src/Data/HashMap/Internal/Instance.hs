{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase   #-}

module Data.HashMap.Internal.Instance where

import Control.DeepSeq (NFData (..))
import Data.Bifunctor (Bifunctor (..))
import Data.HashMap.Internal.Base (HashMap (..), defH, insert, size, fromList)
import Data.HashMap.Internal.Basic (union, map, foldrWithKey, toList)
import qualified Data.HashMap.Internal.Base as HB (null)
import Data.HashMap.Internal.Class (DoubleHashable)
import Data.List (intercalate)
import qualified Data.Primitive.Array as A
import Data.Foldable (Foldable(..))
import Data.Maybe (mapMaybe)
import Data.Bifoldable (Bifoldable (bifoldr))
import Text.Read (Read(readPrec, readListPrec), Lexeme (Ident), parens, prec, lexP)

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


instance Foldable (HashMap k) where
  foldr :: (a -> b -> b) -> b -> HashMap k a -> b
  foldr f z = foldr (fun f) z . buckets
  foldr' :: (a -> b -> b) -> b -> HashMap k a -> b
  foldr' f z = foldr' (fun f) z . buckets
  foldl :: (b -> a -> b) -> b -> HashMap k a -> b
  foldl = foldr . flip
  foldl' :: (b -> a -> b) -> b -> HashMap k a -> b
  foldl' = foldr . flip


instance Functor (HashMap k) where
  fmap :: (a -> b) -> HashMap k a -> HashMap k b
  fmap = Data.HashMap.Internal.Basic.map

instance (Eq k, Eq v) => Eq (HashMap k v) where
  (==) :: HashMap k v -> HashMap k v -> Bool
  (==) h1 h2 = size h1 == size h2
    --where equalHelper h1 h2 = if size h1 < size h2 then notEqual h2 h1 else


instance (NFData k, NFData v) => NFData (HashMap k v) where
  rnf :: HashMap k v -> ()
  rnf (HashMap size v) = rnf size `seq` rnf v


instance DoubleHashable k => Semigroup (HashMap k v) where
  (<>) :: HashMap k v -> HashMap k v -> HashMap k v
  (<>) = union

instance DoubleHashable k => Monoid (HashMap k v) where
  mempty :: HashMap k v
  mempty = HB.null