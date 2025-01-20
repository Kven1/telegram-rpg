module Domain.Entity.Health (Health (..)) where

import Data.Eq qualified as Eq
import Data.Int qualified as Int
import Data.Ord qualified as Ord
import Data.Semigroup ((<>))
import Domain.Entity.Damageable qualified as Damageable
import Domain.Entity.Healable qualified as Healable
import GHC.Num ((+), (-))
import PrettyPrint qualified
import Text.Read qualified as Read
import Text.Show qualified as Show

data Health
    = Health {current :: Int.Int32, max :: Int.Int32}
    deriving (Show.Show, Eq.Eq, Read.Read)

instance Damageable.Damageable Health where
    hp = current
    takeDamage health damage = health{current = Ord.max (current health - damage) 0}

instance Healable.Healable Health where
    heal by health = health{current = Ord.min (current health + by) (max health)}

instance PrettyPrint.PrettyPrint Health where
    prettyPrint (Health c m) = Show.show c <> "/" <> Show.show m <> " HP"
