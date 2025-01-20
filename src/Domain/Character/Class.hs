{-# LANGUAGE UndecidableInstances #-}

module Domain.Character.Class where

import Control.Category ((>>>))
import Data.Eq qualified as Eq
import Data.Functor ((<&>))
import Data.Ord qualified as Ord
import Data.String qualified as String
import Data.Text qualified as Text
import Database.Beam qualified as Beam
import Database.Beam.Backend qualified as Beam.Backend
import Database.Beam.Backend.SQL qualified as Beam.Backend.SQL
import Database.Beam.Migrate qualified as Beam.Migrate
import Database.Beam.Sqlite qualified as Beam.Sqlite
import Database.Beam.Sqlite.Syntax qualified as Beam.Sqlite.Syntax
import Text.Read qualified as Read
import Text.Show qualified as Show

data Class
    = Fighter
    | Ranger
    deriving (Show.Show, Eq.Eq, Read.Read)

deriving instance Ord.Ord Class

instance (Beam.Backend.HasSqlValueSyntax be String.String) => Beam.Backend.HasSqlValueSyntax be Class where
    sqlValueSyntax = Beam.Backend.autoSqlValueSyntax

instance Beam.Backend.SQL.FromBackendRow Beam.Sqlite.Sqlite Class where
    fromBackendRow = Beam.Backend.SQL.fromBackendRow <&> (Text.unpack >>> Read.read)

instance Beam.Migrate.HasDefaultSqlDataType Beam.Sqlite.Sqlite Class where
    defaultSqlDataType _ _ _ = Beam.Sqlite.Syntax.sqliteTextType
