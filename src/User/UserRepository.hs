module User.UserRepository where

import DB qualified
import Data.Function (($), (&))
import Data.Maybe qualified as Maybe
import Data.Text qualified as Text
import Data.Time qualified as Time
import Database.Beam ((==.))
import Database.Beam.Backend.SQL qualified as Beam.Backend.SQL
import Database.Beam.Query qualified as Q
import Database.Beam.Sqlite qualified as Beam.Sqlite
import Domain.User qualified as User
import GHC.Float qualified as Float

findUserById :: (Beam.Backend.SQL.MonadBeam Beam.Sqlite.Sqlite m) => Float.Double -> m (Maybe.Maybe User.User)
findUserById id = Q.runSelectReturningOne $ Q.select $ Q.all_ fromUsers & Q.filter_ byId
  where
    fromUsers = DB.users DB.db
    byId user = User.uId user ==. Q.val_ id

createUser :: (Beam.Backend.SQL.MonadBeam Beam.Sqlite.Sqlite m) => Float.Double -> Text.Text -> Time.LocalTime -> m ()
createUser id name createdAt =
    Q.runInsert $ Q.insert intoUsers $ Q.insertValues [user]
  where
    intoUsers = DB.users DB.db
    user = User.User{User.uId = id, User.uName = name, User.uCreatedAt = createdAt}
