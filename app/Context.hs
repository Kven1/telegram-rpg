module Context (Context (..), ContextM, withCtx) where

import AI.Chat qualified as Chat
import Control.Monad.Reader qualified as Monad.Reader
import Database.SQLite.Simple qualified as SQLite
import Model qualified
import Telegram.Bot.Simple qualified as TelegramBot

data Context = Context {conn :: SQLite.Connection, chat :: Chat.Connection}
type ContextM a = Monad.Reader.ReaderT Context TelegramBot.BotM a

--- | Wraps Telegram.Bot.Simple's context with our own.
withCtx :: SQLite.Connection -> Chat.Connection -> Model.Model -> (Model.Model -> Monad.Reader.ReaderT Context m a) -> m a
withCtx dbConn chatConn model f = Monad.Reader.runReaderT (f model) Context{conn = dbConn, chat = chatConn}
