{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Main (main) where

import Control.Applicative         (Const (..))
import Data.Bifoldable
import Data.Function               (on)
import Data.Functor.Identity       (Identity (..))
import Data.Hashable               (Hashable (hashWithSalt))
import Data.Ord                    (comparing)
import Test.QuickCheck             (Arbitrary (..), Fun, Property, (===), (==>), pattern Fn, pattern Fn2, pattern Fn3)
import Test.QuickCheck.Poly        (A, B, C)
import Test.Tasty                  (TestTree, testGroup, defaultMain)
import Test.Tasty.QuickCheck       (testProperty)
import Util.Key

import           Data.HashMap (HashMap)
import qualified Data.HashMap as HM
import qualified Data.Map.Lazy     as M

import qualified Data.Foldable   as Foldable
import qualified Data.List       as List
import qualified Test.QuickCheck as QC
import Data.HashMap.Internal.Debug (Validity(Valid), valid)


instance (Eq k, Arbitrary k, Arbitrary v, HM.DoubleHashable k) => Arbitrary (HashMap k v) where
  arbitrary = HM.fromList <$> arbitrary
  shrink = fmap HM.fromList . shrink . HM.toList

------------------------------------------------------------------------
-- Helpers

type HMK  = HashMap Key
type HMKI = HMK Int

isValid :: (Eq k, Hashable k, Show k) => HashMap k v -> Property
isValid m = valid m === Valid

sortByKey = List.sortBy (compare `on` fst)

toOrdMap :: Ord k => HashMap k v -> M.Map k v
toOrdMap = M.fromList . HM.toList

main :: IO ()
main = defaultMain tests

------------------------------------------------------------------------
-- Test list

tests :: TestTree
tests =
  testGroup
    "Data.HashMap"
    [
    -- Instances
      testGroup "instances"
      [ testGroup "Eq"
        [ testProperty "==" $
          \(x :: HMKI) y -> (x == y) === (toOrdMap x == toOrdMap y)
        , testProperty "/=" $
          \(x :: HMKI) y -> (x == y) === (toOrdMap x == toOrdMap y)
        ]
{-      , testGroup "Ord"
        [ testProperty "compare reflexive" $
          \(m :: HMKI) -> compare m m === EQ
        , testProperty "compare transitive" $
          \(x :: HMKI) y z -> case (compare x y, compare y z) of
            (EQ, o)  -> compare x z === o
            (o,  EQ) -> compare x z === o
            (LT, LT) -> compare x z === LT
            (GT, GT) -> compare x z === GT
            (LT, GT) -> QC.property True -- ys greater than xs and zs.
            (GT, LT) -> QC.property True
        , testProperty "compare antisymmetric" $
          \(x :: HMKI) y -> case (compare x y, compare y x) of
            (EQ, EQ) -> True
            (LT, GT) -> True
            (GT, LT) -> True
            _        -> False
        , testProperty "Ord => Eq" $
          \(x :: HMKI) y -> case (compare x y, x == y) of
            (EQ, True)  -> True
            (LT, False) -> True
            (GT, False) -> True
            _           -> False
        ]-}
      , testProperty "Read/Show" $
        \(x :: HMKI) -> x === read (show x)
      , testProperty "Functor" $
        \(x :: HMKI) (Fn f :: Fun Int Int) ->
          toOrdMap (fmap f x) === fmap f (toOrdMap x)
      , testProperty "Foldable" $
        \(x :: HMKI) ->
          let f = List.sort . Foldable.foldr (:) []
          in  f x === f (toOrdMap x)
{-      , testProperty "Hashable" $
        \(xs :: [(Key, Int)]) is salt ->
          let xs' = List.nubBy (\(k,_) (k',_) -> k == k') xs
              -- Shuffle the list using indexes in the second
              shuffle :: [Int] -> [a] -> [a]
              shuffle idxs = List.map snd
                           . List.sortBy (comparing fst)
                           . List.zip (idxs ++ [List.maximum (0:is) + 1 ..])
              ys = shuffle is xs'
              x = HM.fromList xs'
              y = HM.fromList ys
          in  x == y ==> hashWithSalt salt x === hashWithSalt salt y
      ]-}
    -- Construction
    , testGroup "empty"
      [ testProperty "valid" $ QC.once $
        isValid (HM.null :: HMKI)
      ]
    , testGroup "singleton"
      [ testProperty "valid" $
        \(k :: Key) (v :: A) -> isValid (HM.singleton k v)
      ]
    -- Basic interface
    , testProperty "size" $
      \(x :: HMKI) -> HM.size x === M.size (toOrdMap x)
    , testProperty "member" $
      \(k :: Key) (m :: HMKI) -> HM.member k m === M.member k (toOrdMap m)
    , testProperty "lookup" $
      \(k :: Key) (m :: HMKI) -> HM.lookup k m === M.lookup k (toOrdMap m)
    , testProperty "!?" $
      \(k :: Key) (m :: HMKI) -> m HM.!? k === M.lookup k (toOrdMap m)
    , testGroup "insert"
      [ testProperty "model" $
        \(k :: Key) (v :: Int) x ->
          let y = HM.insert k v x
          in  toOrdMap y === M.insert k v (toOrdMap x)
      , testProperty "valid" $
        \(k :: Key) (v :: Int) x -> isValid (HM.insert k v x)
      ]
    , testGroup "insertWith"
      [ testProperty "insertWith" $
        \(Fn2 f) k v (x :: HMKI) ->
          toOrdMap (HM.insertWith f k v x) === M.insertWith f k v (toOrdMap x)
      , testProperty "valid" $
        \(Fn2 f) k v (x :: HMKI) -> isValid (HM.insertWith f k v x)
      ]
    , testGroup "delete"
      [ testProperty "model" $
        \(k :: Key) (x :: HMKI) ->
          let y = HM.delete k x
          in  toOrdMap y === M.delete k (toOrdMap x)
      , testProperty "valid" $
        \(k :: Key) (x :: HMKI) -> isValid (HM.delete k x)
      ]
    , testGroup "alterF"
      [ testGroup "model"
        [ -- We choose the list functor here because we don't fuss with
          -- it in alterF rules and because it has a sufficiently interesting
          -- structure to have a good chance of breaking if something is wrong.
          testProperty "[]" $
          \(Fn f :: Fun (Maybe A) [Maybe A]) k (x :: HMK A) ->
            map toOrdMap (HM.alterF f k x) === M.alterF f k (toOrdMap x)
        , testProperty "adjust" $
          \(Fn f) k (x :: HMKI) ->
            let g = Identity . fmap f
            in  fmap toOrdMap (HM.alterF g k x) === M.alterF g k (toOrdMap x)
        , testProperty "insert" $
          \v k (x :: HMKI) ->
            let g = const . Identity . Just $ v
            in  fmap toOrdMap (HM.alterF g k x) === M.alterF g k (toOrdMap x)
        , testProperty "insertWith" $
          \(Fn f) k v (x :: HMKI) ->
            let g = Identity . Just . maybe v f
            in  fmap toOrdMap (HM.alterF g k x) === M.alterF g k (toOrdMap x)
        , testProperty "delete" $
          \k (x :: HMKI) ->
            let f = const (Identity Nothing)
            in  fmap toOrdMap (HM.alterF f k x) === M.alterF f k (toOrdMap x)
        , testProperty "lookup" $
          \(Fn f :: Fun (Maybe A) B) k (x :: HMK A) ->
            let g = Const . f
            in  fmap toOrdMap (HM.alterF g k x) === M.alterF g k (toOrdMap x)
        ]
      , testProperty "valid" $
        \(Fn f :: Fun (Maybe A) [Maybe A]) k (x :: HMK A) ->
          let ys = HM.alterF f k x
          in  map valid ys === (Valid <$ ys)
      ] 
    -- Combine
    , testGroup "union"
      [ testProperty "model" $
        \(x :: HMKI) y ->
          let z = HM.union x y
          in  toOrdMap z === M.union (toOrdMap x) (toOrdMap y)
      , testProperty "valid" $
        \(x :: HMKI) y -> isValid (HM.union x y)
      ]
    , testGroup "unionWith"
      [ testProperty "model" $
        \(Fn2 f) (x :: HMKI) y ->
          toOrdMap (HM.unionWith f x y) === M.unionWith f (toOrdMap x) (toOrdMap y)
      , testProperty "valid" $
        \(Fn2 f) (x :: HMKI) y -> isValid (HM.unionWith f x y)
      ]
    , testGroup "unionWithKey"
      [ testProperty "model" $
        \(Fn3 f) (x :: HMKI) y ->
          toOrdMap (HM.unionWithKey f x y) === M.unionWithKey f (toOrdMap x) (toOrdMap y)
      , testProperty "valid" $
        \(Fn3 f) (x :: HMKI) y -> isValid (HM.unionWithKey f x y)
      ]
    , testGroup "difference"
      [ testProperty "model" $
        \(x :: HMKI) (y :: HMKI) ->
          toOrdMap (HM.difference x y) === M.difference (toOrdMap x) (toOrdMap y)
      , testProperty "valid" $
        \(x :: HMKI) (y :: HMKI) -> isValid (HM.difference x y)
      ]
    , testGroup "intersection"
      [ testProperty "model" $
        \(x :: HMKI) (y :: HMKI) ->
          toOrdMap (HM.intersection x y) === M.intersection (toOrdMap x) (toOrdMap y)
      , testProperty "valid" $
        \(x :: HMKI) (y :: HMKI) ->
          isValid (HM.intersection x y)
      ]
    , testGroup "intersectionWith"
      [ testProperty "model" $
        \(Fn2 f :: Fun (A, B) C) (x :: HMK A) (y :: HMK B) ->
          toOrdMap (HM.intersectionWith f x y) === M.intersectionWith f (toOrdMap x) (toOrdMap y)
      , testProperty "valid" $
        \(Fn2 f :: Fun (A, B) C) (x :: HMK A) (y :: HMK B) ->
          isValid (HM.intersectionWith f x y)
      ]
    , testGroup "intersectionWithKey"
      [ testProperty "model" $
        \(Fn3 f :: Fun (Key, A, B) C) (x :: HMK A) (y :: HMK B) ->
          toOrdMap (HM.intersectionWithKey f x y)
          ===
          M.intersectionWithKey f (toOrdMap x) (toOrdMap y)
      , testProperty "valid" $
        \(Fn3 f :: Fun (Key, A, B) C) (x :: HMK A) (y :: HMK B) ->
          isValid (HM.intersectionWithKey f x y)
      ]
    -- Transformations
    , testGroup "map"
      [ testProperty "model" $
        \(Fn f :: Fun A B) (m :: HMK A) -> toOrdMap (HM.map f m) === M.map f (toOrdMap m)
      , testProperty "valid" $
        \(Fn f :: Fun A B) (m :: HMK A) -> isValid (HM.map f m)
      ]
{-    , testGroup "traverseWithKey"
      [ testProperty "model" $ QC.mapSize (`div` 8) $
        \(x :: HMKI) ->
          let f k v = [keyToInt k + v + 1, keyToInt k + v + 2]
              ys = HM.traverseWithKey f x
          in  List.sort (fmap toOrdMap ys) === List.sort (M.traverseWithKey f (toOrdMap x))
      , testProperty "valid" $ QC.mapSize (`div` 8) $
        \(x :: HMKI) ->
          let f k v = [keyToInt k + v + 1, keyToInt k + v + 2]
              ys = HM.traverseWithKey f x
          in  fmap valid ys === (Valid <$ ys)
      ]-}
    -- Folds
    , testProperty "foldr" $
      \(m :: HMKI) -> List.sort (HM.foldr (:) [] m) === List.sort (M.foldr (:) [] (toOrdMap m))
    , testProperty "foldl" $
      \(m :: HMKI) ->
        List.sort (HM.foldl (flip (:)) [] m) === List.sort (M.foldl (flip (:)) [] (toOrdMap m))
    , testProperty "foldrWithKey" $
      \(m :: HMKI) ->
        let f k v z = (k, v) : z
        in  sortByKey (HM.foldrWithKey f [] m) === sortByKey (M.foldrWithKey f [] (toOrdMap m))
    , testProperty "foldl'" $
      \(m :: HMKI) ->
        List.sort (HM.foldl' (flip (:)) [] m) === List.sort (M.foldl' (flip (:)) [] (toOrdMap m))
    , testProperty "foldr'" $
      \(m :: HMKI) -> List.sort (HM.foldr' (:) [] m) === List.sort (M.foldr' (:) [] (toOrdMap m))
    , testProperty "foldMapWithKey" $
      \(m :: HMKI) ->
        let f k v = [(k, v)]
        in  sortByKey (HM.foldMapWithKey f m) === sortByKey (M.foldMapWithKey f (toOrdMap m))
    -- Filter
    , testGroup "filter"
      [ testProperty "model" $
        \(Fn p) (m :: HMKI) -> toOrdMap (HM.filter p m) === M.filter p (toOrdMap m)
      , testProperty "valid" $
        \(Fn p) (m :: HMKI) -> isValid (HM.filter p m)
      ]
    , testGroup "filterWithKey"
      [ testProperty "model" $
        \(Fn2 p) (m :: HMKI) ->
          toOrdMap (HM.filterWithKey p m) === M.filterWithKey p (toOrdMap m)
      , testProperty "valid" $
        \(Fn2 p) (m :: HMKI) -> isValid (HM.filterWithKey p m)
      ]
    , testGroup "mapMaybe"
      [ testProperty "model" $
        \(Fn f :: Fun A (Maybe B)) (m :: HMK A) ->
          toOrdMap (HM.mapMaybe f m) === M.mapMaybe f (toOrdMap m)
      , testProperty "valid" $
        \(Fn f :: Fun A (Maybe B)) (m :: HMK A) -> isValid (HM.mapMaybe f m)
      ]
    , testGroup "mapMaybeWithKey"
      [ testProperty "model" $
        \(Fn2 f :: Fun (Key, A) (Maybe B)) (m :: HMK A) ->
          toOrdMap (HM.mapMaybeWithKey f m) === M.mapMaybeWithKey f (toOrdMap m)
      , testProperty "valid" $
        \(Fn2 f :: Fun (Key, A) (Maybe B)) (m :: HMK A) ->
          isValid (HM.mapMaybeWithKey f m)
      ]
    -- Conversions
    , testProperty "elems" $
      \(m :: HMKI) -> List.sort (HM.elems m) === List.sort (M.elems (toOrdMap m))
    , testProperty "keys" $
      \(m :: HMKI) -> List.sort (HM.keys m) === List.sort (M.keys (toOrdMap m))
    , testGroup "fromList"
      [ testProperty "model" $
        \(kvs :: [(Key, Int)]) -> toOrdMap (HM.fromList kvs) === M.fromList kvs
      , testProperty "valid" $
        \(kvs :: [(Key, Int)]) -> isValid (HM.fromList kvs)
      ]
{-    , testGroup "fromListWith"
      [ testProperty "model" $
        \(kvs :: [(Key, Int)]) ->
          let kvsM = map (fmap Leaf) kvs
          in  toOrdMap (HM.fromListWith Op kvsM) === M.fromListWith Op kvsM
      , testProperty "valid" $
        \(Fn2 f) (kvs :: [(Key, A)]) -> isValid (HM.fromListWith f kvs)
      ]-}
    , testProperty "toList" $
      \(m :: HMKI) -> List.sort (HM.toList m) === List.sort (M.toList (toOrdMap m))
    ]
  ]