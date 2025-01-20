{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import AI.Chat qualified as Chat
import Action qualified
import Configuration.Dotenv qualified as Dotenv
import Context qualified
import Control.Monad ((>>=))
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.Reader qualified as Reader
import DB qualified
import Data.Function (($))
import Data.Function qualified as Function
import Data.Functor ((<$>))
import Data.Maybe qualified as Maybe
import Data.String qualified as String
import Database.SQLite.Simple qualified as SQLite
import Model qualified
import Network.HTTP.Req qualified as Req
import System.Environment qualified
import System.IO qualified as IO
import Telegram.Bot.API qualified as Telegram
import Telegram.Bot.Simple ((<#))
import Telegram.Bot.Simple qualified as TelegramBot
import Telegram.Bot.Simple.Conversation qualified as Conversation
import Telegram.Bot.Simple.Debug qualified as TelegramBot.Debug
import User.Telegram qualified

-- | Handle actions using ContextM monad.
botHandler :: SQLite.Connection -> Chat.Connection -> Action.Action -> Model.Model -> TelegramBot.Eff Action.Action Model.Model
botHandler dbConn chatConn action model =
    updatedModel <# do
        user <- Reader.reader TelegramBot.botContextUser
        MonadIO.liftIO $ User.Telegram.trackUserAction dbConn (Telegram.userId user) action

        Action.HandledAction <$> Context.withCtx dbConn chatConn updatedModel (`Action.handleAction` action)
  where
    updatedModel = Action.handleMutation model action

-- | Bot application.
bot :: SQLite.Connection -> Chat.Connection -> TelegramBot.BotApp Model.Model Action.Action
bot dbConn chatConn =
    TelegramBot.BotApp
        { TelegramBot.botInitialModel = Model.emptyUserForm
        , TelegramBot.botAction = Function.flip Action.handleUpdate
        , TelegramBot.botHandler = botHandler dbConn chatConn
        , TelegramBot.botJobs = [TelegramBot.BotJob "*/5 * * * *" (\model -> model <# TelegramBot.replyText "Эхо моих коридоров давно не слышало твоих шагов... Подземелья скучают по тебе.")]
        }

-- | Run bot with a given 'Telegram.Token'.
run :: SQLite.Connection -> Chat.Connection -> Telegram.Token -> IO.IO ()
run dbConn chatConn token = do
    env <- Telegram.defaultTelegramClientEnv token
    Function.flip TelegramBot.startBot_ env $
        TelegramBot.useLatestUpdateInJobs $
            TelegramBot.Debug.traceBotDefault $
                Conversation.conversationBot Telegram.updateChatId $
                    bot dbConn chatConn

main :: IO.IO ()
main = do
    Dotenv.loadFile Dotenv.defaultConfig

    openaiKey <- System.Environment.getEnv "OPENAI_KEY"
    let chatConn = Chat.Connection (Req.https (String.fromString "api.openai.com")) (Maybe.Just openaiKey) "gpt-4o-mini"

    dbConn <- SQLite.open "tg-rpg.db"
    DB.runQuery dbConn DB.tryMigrate

    IO.putStrLn "Starting bot"
    TelegramBot.getEnvToken "TELEGRAM_BOT_TOKEN" >>= run dbConn chatConn
