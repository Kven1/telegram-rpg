module Domain.Entity.Damageable (Damageable (..)) where

import Data.Bool qualified as Bool
import Data.Int qualified as Int
import Data.Ord ((<=))

class Damageable entity where
    hp :: entity -> Int.Int32
    takeDamage :: entity -> Int.Int32 -> entity
    isDead :: entity -> Bool.Bool
    isDead entity = hp entity <= 0
