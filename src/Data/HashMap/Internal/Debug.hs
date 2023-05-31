module Data.HashMap.Internal.Debug where
import Data.HashMap.Internal.Base (HashMap (..))
import Data.List (intercalate)

showDebug :: (Show k, Show v) => HashMap k v -> String
showDebug (HashMap v1 v2 v3 s cur) = intercalate "\n" [
    show v1, show v2, show $ take 3 v3, show s, show cur
  ]