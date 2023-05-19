{-# LANGUAGE InstanceSigs #-}
module Data.HashMap.Internal.Instance where
import Data.HashMap.Internal.Base (HashMap (..))
import Data.HashMap.Internal.Class (DoubleHashable)

instance (DoubleHashable k, Show k, Show v) => Show (HashMap k v) where
  show :: HashMap k v -> String
  show (HashMap size v) = foldl (\pre p -> case p of
    Just (Just (k, v)) -> pre ++ show k ++ " => " ++ show v ++ ", " 
    _ -> pre
    ) "{" v ++ "}"