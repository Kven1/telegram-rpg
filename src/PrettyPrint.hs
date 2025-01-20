module PrettyPrint where

import Data.String qualified as String

class PrettyPrint a where
    prettyPrint :: a -> String.String
