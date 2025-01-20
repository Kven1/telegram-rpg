{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.User (UserT (..), User, UserId) where

import Control.Category ((>>>))
import Data.Eq qualified as Eq
import Data.Functor.Identity qualified as Identity
import Data.Text qualified as Text
import Data.Time qualified as Time
import Database.Beam qualified as Beam
import Domain.Character.Class qualified
import GHC.Float qualified as Float
import GHC.Generics qualified as Generics
import Text.Show qualified as Show

data UserT f
    = User
    { uId :: Beam.Columnar f Float.Double
    , uName :: Beam.Columnar f Text.Text
    , uClass :: Beam.Columnar f Domain.Character.Class.Class
    , uCreatedAt :: Beam.Columnar f Time.LocalTime
    }
    deriving (Generics.Generic)

type User = UserT Identity.Identity

type UserId = Beam.PrimaryKey UserT Identity.Identity

deriving instance Show.Show User

deriving instance Eq.Eq User

instance Beam.Beamable UserT

instance Beam.Table UserT where
    data PrimaryKey UserT f = UserId (Beam.Columnar f Float.Double) deriving (Generics.Generic, Beam.Beamable)
    primaryKey :: UserT column -> Beam.PrimaryKey UserT column
    primaryKey = uId >>> UserId
