module Data.HashMap(
    DoubleHashable(..),
    HashMap,
    singleton,
    null,
    size,
    insert, 
    insertWith,
    fromList, 
    lookup, 
    delete    
) where

import Data.HashMap.Internal.Class(DoubleHashable(..))
import Data.HashMap.Internal.Base(HashMap, singleton, null, size, insert, insertWith, lookup, delete, fromList)
import Data.HashMap.Internal.Instance
import Prelude hiding (lookup, null)