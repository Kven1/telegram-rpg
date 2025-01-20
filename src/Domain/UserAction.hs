{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.UserAction (UserActionT (..), UserAction, UserActionId) where

import Control.Category ((>>>))
import Data.Functor.Identity qualified as Identity
import Data.Int qualified as Int
import Data.Text qualified as Text
import Data.Time qualified as Time
import Database.Beam qualified as Beam
import Database.Beam.Backend qualified as Beam.Backend
import GHC.Float qualified as Float
import GHC.Generics qualified as Generics
import GHC.Show qualified as Show
import Prelude qualified

data UserActionT f
    = UserAction
    { uaId :: Beam.Columnar f (Beam.Backend.SqlSerial Int.Int32)
    , uaUserId :: Beam.Columnar f Float.Double
    , uaData :: Beam.Columnar f Text.Text
    , uaCreatedAt :: Beam.Columnar f Time.LocalTime
    }
    deriving (Generics.Generic)

type UserAction = UserActionT Identity.Identity

type UserActionId = Beam.PrimaryKey UserActionT Identity.Identity

deriving instance Show.Show UserAction

deriving instance Prelude.Eq UserAction

instance Beam.Beamable UserActionT

instance Beam.Table UserActionT where
    data PrimaryKey UserActionT f
        = UserActionId (Beam.Columnar f (Beam.Backend.SqlSerial Int.Int32))
        deriving (Generics.Generic, Beam.Beamable)

    primaryKey :: UserActionT column -> Beam.PrimaryKey UserActionT column
    primaryKey = uaId >>> UserActionId
