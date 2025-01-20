{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module DB (Db (..), dbChecked, db, tryMigrate, runQuery) where

import Control.Monad.IO.Class qualified as MonadIO
import Data.Function (($))
import Database.Beam qualified as Beam
import Database.Beam.Migrate qualified as Beam.Migrate
import Database.Beam.Migrate.Simple qualified as Beam.Migrate.Simple
import Database.Beam.Sqlite qualified as Beam.Sqlite
import Database.Beam.Sqlite.Migrate qualified as Beam.Sqlite.Migrate
import Database.SQLite.Simple qualified as SQLite
import Domain.User qualified as User
import Domain.UserAction qualified as UserAction
import GHC.Generics qualified as Generics
import System.IO qualified as IO

data Db f = Db
    { users :: f (Beam.TableEntity User.UserT)
    , userActions :: f (Beam.TableEntity UserAction.UserActionT)
    }
    deriving (Generics.Generic, Beam.Database be)

dbChecked :: Beam.Migrate.Simple.CheckedDatabaseSettings Beam.Sqlite.Sqlite Db
dbChecked = Beam.Migrate.defaultMigratableDbSettings

db :: Beam.DatabaseSettings Beam.Sqlite.Sqlite Db
db = Beam.Migrate.unCheckDatabase dbChecked

migrate :: Beam.Sqlite.SqliteM ()
migrate = Beam.Migrate.Simple.autoMigrate Beam.Sqlite.Migrate.migrationBackend dbChecked

verifyMigrations :: Beam.Sqlite.SqliteM Beam.Migrate.Simple.VerificationResult
verifyMigrations = Beam.Migrate.Simple.verifySchema Beam.Sqlite.Migrate.migrationBackend dbChecked

tryMigrate :: Beam.Sqlite.SqliteM ()
tryMigrate = do
    result <- verifyMigrations
    case result of
        Beam.Migrate.Simple.VerificationSucceeded -> MonadIO.liftIO $ IO.putStrLn "Database is up-to-date"
        Beam.Migrate.Simple.VerificationFailed _ -> do
            MonadIO.liftIO $ IO.putStrLn "Trying to apply migrations..."
            migrate

runQuery :: SQLite.Connection -> Beam.Sqlite.SqliteM a -> IO.IO a
runQuery = Beam.Sqlite.runBeamSqlite
