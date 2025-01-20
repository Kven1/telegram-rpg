module Domain.Entity.Healable (Healable (..)) where

import Data.Int qualified as Int

class Healable a where
    heal :: Int.Int32 -> a -> a
