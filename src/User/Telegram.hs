module User.Telegram (getCurrentUser, getCurrentTelegramUser, saveNewTelegramUser, tgUserIdToDouble, trackUserAction) where

import Control.Applicative qualified as Applicative
import Control.Monad ((>=>), (>>=))
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.Reader qualified as Monad.Reader
import DB qualified
import Data.Function (($), (&))
import Data.Functor ((<&>))
import Data.Maybe qualified as Maybe
import Data.String qualified as String
import Data.Time qualified as Time
import Data.Time.Clock qualified as Clock
import Data.Time.Clock qualified as Time.Clock
import Database.SQLite.Simple qualified as SQLite
import Domain.User qualified as User
import GHC.Float qualified as FLoat
import GHC.Real qualified as Real
import System.IO qualified as IO
import Telegram.Bot.API qualified as Telegram
import Telegram.Bot.Simple qualified as TelegramBot
import Text.Show qualified as Show
import User.UserRepository qualified as UserRepository
import UserAction.UserActionRepository qualified as UserActionRepository

tgUserIdToDouble :: Telegram.UserId -> FLoat.Double
tgUserIdToDouble (Telegram.UserId id) = Real.fromIntegral id

getCurrentTelegramUser :: TelegramBot.BotM Telegram.User
getCurrentTelegramUser = do
    maybeTgUpdate <- Monad.Reader.reader TelegramBot.botContextUpdate
    let maybeFrom = maybeTgUpdate >>= (Telegram.updateMessage >=> Telegram.messageFrom)
    MonadIO.liftIO $ case maybeFrom of
        Maybe.Just user -> Applicative.pure user
        Maybe.Nothing -> Monad.fail "No update"

getCurrentUser :: SQLite.Connection -> TelegramBot.BotM (Maybe.Maybe User.User)
getCurrentUser conn = do
    tgUser <- getCurrentTelegramUser
    tgUser
        & Telegram.userId
        & tgUserIdToDouble
        & UserRepository.findUserById
        & DB.runQuery conn
        & MonadIO.liftIO

saveNewTelegramUser :: SQLite.Connection -> TelegramBot.BotM ()
saveNewTelegramUser conn = do
    tgUser <- getCurrentTelegramUser

    localTime <- MonadIO.liftIO $ do
        now <- Clock.getCurrentTime
        currentTimeZone <- Time.getCurrentTimeZone
        Applicative.pure $ Time.utcToLocalTime currentTimeZone now

    let userId = tgUser & Telegram.userId & tgUserIdToDouble
    let userName = Telegram.userFirstName tgUser
    MonadIO.liftIO $ DB.runQuery conn $ UserRepository.createUser userId userName localTime

trackUserAction :: (Show.Show a) => SQLite.Connection -> Telegram.UserId -> a -> IO.IO ()
trackUserAction conn userId action = do
    timezone <- MonadIO.liftIO Time.getCurrentTimeZone
    timeNow <- MonadIO.liftIO Time.Clock.getCurrentTime <&> Time.utcToLocalTime timezone

    MonadIO.liftIO $
        DB.runQuery conn $
            UserActionRepository.createUserAction
                (User.Telegram.tgUserIdToDouble userId)
                (String.fromString $ Show.show action)
                timeNow
