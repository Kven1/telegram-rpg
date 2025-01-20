{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module AI.Chat (completions, runM, ChatM, Connection (..)) where

import AI.Completion.Message qualified as Message
import AI.Completion.Request qualified as Completion.Request
import AI.Completion.Response qualified as Completion.Response
import Control.Applicative ((<$>))
import Control.Category ((>>>))
import Control.Monad.Reader qualified as Monad.Reader
import Data.Function (($))
import Data.Functor ((<&>))
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Monoid qualified as Monoid
import Data.Semigroup ((<>))
import Data.String qualified as String
import Network.HTTP.Req ((/:))
import Network.HTTP.Req qualified as Req
import System.IO qualified as IO

data Connection = Connection {host :: Req.Url 'Req.Https, token :: Maybe.Maybe String.String, model :: String.String}
type ChatM a = Monad.Reader.ReaderT Connection IO.IO a

runM :: Connection -> ChatM a -> IO.IO a
runM chatApi m = Monad.Reader.runReaderT m chatApi

completions :: String.String -> ChatM String.String
completions prompt = do
    host <- Monad.Reader.asks host
    token <- Monad.Reader.asks token
    model <- Monad.Reader.asks model

    request host (String.fromString <$> token) model
        <&> (Req.responseBody >>> Completion.Response.choices >>> List.head >>> Completion.Response.message >>> Message.content)
  where
    request host token model =
        Req.runReq Req.defaultHttpConfig
            $ Req.req
                Req.POST
                (host /: "v1" /: "chat" /: "completions")
                (Req.ReqBodyJson $ payload model)
                Req.jsonResponse
            $ headers token
    payload model =
        Completion.Request.CompletionRequest
            { Completion.Request.model = model
            , Completion.Request.messages =
                [ Message.Message
                    { Message.role = "user"
                    , Message.content = prompt
                    }
                ]
            , Completion.Request.temperature = 0.7
            }
    headers = Maybe.maybe Monoid.mempty (\key -> Req.header "Authorization" ("Bearer " <> key))
