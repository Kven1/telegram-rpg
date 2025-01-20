module UserAction.UserActionRepository (createUserAction, listUserActions) where

import DB qualified
import Data.Function (($))
import Data.Text qualified as Text
import Data.Time qualified as Time
import Database.Beam.Backend.SQL qualified as Beam.Backend.SQL
import Database.Beam.Query qualified as Q
import Database.Beam.Sqlite qualified as Beam.Sqlite
import Domain.UserAction qualified as UserAction
import GHC.Float qualified as Float

createUserAction :: (Beam.Backend.SQL.MonadBeam Beam.Sqlite.Sqlite m) => Float.Double -> Text.Text -> Time.LocalTime -> m ()
createUserAction uaUserId uaData uaCreatedAt =
    Q.runInsert $ Q.insert (DB.userActions DB.db) $ Q.insertExpressions [UserAction.UserAction Q.default_ (Q.val_ uaUserId) (Q.val_ uaData) (Q.val_ uaCreatedAt)]

listUserActions :: (Beam.Backend.SQL.MonadBeam Beam.Sqlite.Sqlite m) => m [UserAction.UserAction]
listUserActions = Q.runSelectReturningList $ Q.select $ Q.all_ (DB.userActions DB.db)
