module Data.HashMap.Internal where
import Data.HashMap.Internal.Class 
import Data.HashMap.Internal.Base as B

member :: (DoubleHashable k, Eq k) => k -> HashMap k v -> Bool
member k m = case B.lookup k m of
    Nothing -> False
    Just _ -> True