module Domain.Enemies (Enemies (..)) where

import Control.Category ((>>>))
import Data.Eq qualified as Eq
import Data.Function qualified as Function
import Data.Int qualified as Int
import Data.Semigroup ((<>))
import Data.String qualified as String
import Domain.Combat qualified as Combat
import Domain.Entity.Attacker qualified as Attacker
import Domain.Entity.Damageable qualified as Damageable
import Domain.Entity.Health qualified as Health
import PrettyPrint qualified
import Text.Read qualified as Read
import Text.Show qualified as Show

data Enemies = Enemies
    { description :: String.String
    , health :: Health.Health
    , damage :: Int.Int32
    }
    deriving (Show.Show, Eq.Eq, Read.Read)

instance Damageable.Damageable Enemies where
    hp = health >>> Health.current
    takeDamage enemies damage = enemies{health = Damageable.takeDamage (health enemies) damage}

instance Attacker.Attacker Enemies where
    attackDamage = damage
    side = Function.const Combat.Enemies

instance PrettyPrint.PrettyPrint Enemies where
    prettyPrint (Enemies desc health dmg) = desc <> "\n\n" <> "У них " <> PrettyPrint.prettyPrint health <> ", " <> Show.show dmg <> " DMG"
