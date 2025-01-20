module Domain.Character (Character (..), chargeGold) where

import Control.Category ((>>>))
import Data.Eq qualified as Eq
import Data.Function qualified as Function
import Data.Int qualified as Int
import Data.Semigroup ((<>))
import Data.String qualified as String
import Domain.Character.Class qualified as Character.Class
import Domain.Character.Inventory qualified as Character.Inventory
import Domain.Combat qualified as Combat
import Domain.Entity.Attacker qualified as Attacker
import Domain.Entity.Damageable qualified as Damageable
import Domain.Entity.Healable qualified as Healable
import Domain.Entity.Health qualified as Health
import Domain.Entity.Looter qualified as Looter
import GHC.Num ((+))
import Text.Show qualified as Show

data Character = Character
    { name :: String.String
    , cClass :: Character.Class.Class
    , health :: Health.Health
    , inventory :: Character.Inventory.Inventory
    }
    deriving (Show.Show, Eq.Eq)

chargeGold :: Int.Int32 -> Character -> Character
chargeGold gold character = character{inventory = Character.Inventory.Inventory newGold newItems}
  where
    charInventory = inventory character
    newGold = Character.Inventory.gold charInventory + gold
    newItems = Character.Inventory.items charInventory

instance Damageable.Damageable Character where
    hp = health >>> Damageable.hp
    takeDamage character damage = character{health = Damageable.takeDamage (health character) damage}

instance Healable.Healable Character where
    heal by character = character{health = Healable.heal by (health character)}

instance Attacker.Attacker Character where
    attackDamage = inventory >>> Character.Inventory.totalDamageBonus
    side = Function.const Combat.Character

instance Looter.Looter Character where
    takeLoot loot character = character{inventory = inventory character <> loot}
