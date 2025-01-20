module Model (Model (..), MessageRequest (..), emptyUserForm, emptyTavern, emptyCombat) where

import Control.Applicative qualified as Applicative
import Data.Maybe qualified as Maybe
import Data.String qualified as String
import Domain.Character qualified
import Domain.Character.Class qualified
import Domain.Character.Inventory qualified as Inventory
import Domain.Combat qualified
import Domain.Combat qualified as Combat
import Domain.Enemies qualified
import Domain.Entity.Health qualified as Health
import GHC.Generics (Generic)
import Text.Read qualified as Read
import Text.Show qualified as Show

data MessageRequest = CharNameRequest deriving (Show.Show, Generic, Read.Read)

data Model
    = UserForm
        { name :: Maybe.Maybe String.String
        , characterClass :: Maybe.Maybe Domain.Character.Class.Class
        , messageRequest :: Maybe.Maybe MessageRequest
        }
    | Tavern
        { character :: Domain.Character.Character
        }
    | Combat
        { initialState :: Domain.Combat.State Domain.Character.Character Domain.Enemies.Enemies
        , combat :: Domain.Combat.Combat Domain.Character.Character Domain.Enemies.Enemies
        }
    deriving (Show.Show)

emptyUserForm :: Model
emptyUserForm =
    Model.UserForm
        { Model.name = Maybe.Nothing
        , Model.characterClass = Maybe.Nothing
        , Model.messageRequest = Maybe.Nothing
        }

emptyTavern :: String.String -> Domain.Character.Class.Class -> Model
emptyTavern name cClass =
    Model.Tavern
        { Model.character =
            Domain.Character.Character
                { Domain.Character.name = name
                , Domain.Character.cClass = cClass
                , Domain.Character.health = Health.Health 5 5
                , Domain.Character.inventory = Inventory.Inventory 0 [Inventory.Item "Деревянный меч" "А ведь те смуглые джентльмены уверяли, что он волшебный..." Inventory.Weapon 1]
                }
        }

emptyCombat :: Domain.Character.Character -> Domain.Enemies.Enemies -> Model
emptyCombat character enemies =
    Model.Combat
        { Model.initialState = initialState
        , Model.combat = Applicative.pure initialState
        }
  where
    initialState = Combat.InProgress character enemies
