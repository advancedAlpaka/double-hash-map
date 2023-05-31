module Data.HashMap.Internal.Debug where
import Data.HashMap.Internal.Base (HashMap (..))
import Data.List (intercalate)
import Data.Hashable (Hashable)
import Test.QuickCheck (Property, (===))

showDebug :: (Show k, Show v) => HashMap k v -> String
showDebug (HashMap s cur) = intercalate "\n" [
    show s, show cur
  ]

data Validity k = Valid
  deriving (Eq, Show)

isValid :: (Eq k, Hashable k, Show k) => HashMap k v -> Property
isValid m = Valid === Valid