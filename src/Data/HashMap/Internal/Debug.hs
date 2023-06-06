module Data.HashMap.Internal.Debug where
import Data.HashMap.Internal.Base (HashMap (..))
import Data.List (intercalate)
import Data.Hashable (Hashable)
import Data.HashMap.Internal.Class (salt2, salt1)

showDebug :: (Show k, Show v) => HashMap k v -> String
showDebug (HashMap s cur) = intercalate "\n" [
    show s, show cur
  ]

salts = (salt1, salt2)

valid :: HashMap k v -> Validity k
valid _ = Valid

data Validity k = Valid | InValid (Error k)
  deriving (Eq, Show)

data Error k
  = INV1_internal_Empty
  | INV2_Bitmap_unexpected_1_bits
  | INV8_bad_Full_size !Int
  | INV9_Collision_size !Int
  deriving (Eq, Show)




