module Data.HashMap(
      HashMap
    , DoubleHashable(..)


      -- * Construction
    , null
    , singleton

      -- * Basic interface
    , empty
    , size
    , member
    , lookup
    , (!?)
    , findWithDefault
    , (!)
    , insert
    , insertWith
    , unsafeInsert
    , delete
    , elems
    , keys

      -- * Combine
      -- ** Union
    , union
    , unionWith
    , unionWithKey

      -- * Transformations
    , map
--    , traverseWithKey
--    , mapKeys
--
--      -- * Difference and intersection
    , difference
--    , differenceWith
    , intersection
    , intersectionWith
    , intersectionWithKey
    
      -- * Folds
    , foldr
    , foldr'
    , foldrWithKey
    , foldl
    , foldl'
    , foldMapWithKey
    
      -- * Filter
    , filter


      -- ** Lists
    , toList
    , fromList
) where

import Data.HashMap.Internal.Class
import Data.HashMap.Internal.Base
import Data.HashMap.Internal.Instance
import Data.HashMap.Internal.Basic
import Prelude hiding (lookup, null, union, unionWithKey, map)
import Data.Foldable (foldl', foldr')
