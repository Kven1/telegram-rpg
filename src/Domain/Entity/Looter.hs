module Domain.Entity.Looter where

import Domain.Character.Inventory qualified as Inventory

class Looter looter where
    takeLoot :: Inventory.Inventory -> looter -> looter
