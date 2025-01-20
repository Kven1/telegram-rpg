module Domain.Entity.Attacker (Attacker (..)) where

import Data.Function (($))
import Data.Int qualified as Int
import Domain.Combat qualified as Combat
import Domain.Entity.Damageable qualified as Damageable
import Domain.Entity.Looter qualified as Looter

class Attacker attacker where
    attackDamage :: attacker -> Int.Int32
    side :: attacker -> Combat.Side
    attack :: (Damageable.Damageable c, Looter.Looter c, Damageable.Damageable e) => attacker -> Combat.Combat c e
    attack attacker = Combat.emitAction $ Combat.Attack (side attacker) (attackDamage attacker)
