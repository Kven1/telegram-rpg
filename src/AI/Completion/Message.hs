module AI.Completion.Message (Message (..)) where

import Data.Aeson.Types qualified as JSON.Types
import Data.String qualified as String
import GHC.Generics qualified as Generic
import Text.Show qualified as Show

data Message = Message
    { role :: String.String
    , content :: String.String
    }
    deriving (Generic.Generic, Show.Show)

instance JSON.Types.ToJSON Message
instance JSON.Types.FromJSON Message
