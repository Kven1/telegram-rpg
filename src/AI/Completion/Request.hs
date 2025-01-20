module AI.Completion.Request (CompletionRequest (..)) where

import AI.Completion.Message qualified as Message
import Data.Aeson.Types qualified as JSON.Types
import Data.String qualified as String
import GHC.Float qualified as Float
import GHC.Generics qualified as Generic
import Text.Show qualified as Show

data CompletionRequest = CompletionRequest
    { model :: String.String
    , messages :: [Message.Message]
    , temperature :: Float.Double
    }
    deriving (Generic.Generic, Show.Show)

instance JSON.Types.ToJSON CompletionRequest
instance JSON.Types.FromJSON CompletionRequest
