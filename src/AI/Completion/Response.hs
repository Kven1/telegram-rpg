module AI.Completion.Response (CompletionResponse (..), Choice (..)) where

import AI.Completion.Message qualified as Message
import Data.Aeson.Types qualified as JSON.Types
import Data.String qualified as String
import GHC.Float qualified as Float
import GHC.Generics qualified as Generic
import Text.Show qualified as Show

data Choice = Choice
    { index :: Float.Double
    , message :: Message.Message
    , finish_reason :: String.String
    }
    deriving (Generic.Generic, Show.Show)

instance JSON.Types.ToJSON Choice
instance JSON.Types.FromJSON Choice

data Usage = Usage
    { prompt_tokens :: Float.Double
    , completion_tokens :: Float.Double
    , total_tokens :: Float.Double
    }
    deriving (Generic.Generic, Show.Show)

instance JSON.Types.ToJSON Usage
instance JSON.Types.FromJSON Usage

data CompletionResponse = CompletionResponse
    { id :: String.String
    , object :: String.String
    , created :: Float.Double
    , model :: String.String
    , system_fingerprint :: String.String
    , choices :: [Choice]
    , usage :: Usage
    }
    deriving (Generic.Generic, Show.Show)

instance JSON.Types.ToJSON CompletionResponse
instance JSON.Types.FromJSON CompletionResponse
