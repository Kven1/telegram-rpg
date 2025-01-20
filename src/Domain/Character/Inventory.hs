module Domain.Character.Inventory (Inventory (..), Item (..), Slot (..), totalDamageBonus) where

import Control.Category ((>>>))
import Data.Eq ((==))
import Data.Eq qualified as Eq
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Functor qualified as Functor
import Data.Int qualified as Int
import Data.List qualified
import Data.Monoid qualified as Monoid
import Data.Semigroup ((<>))
import Data.Semigroup qualified as Semigroup
import Data.String qualified as String
import GHC.Num ((+))
import PrettyPrint qualified
import Text.Printf qualified
import Text.Read qualified as Read
import Text.Show qualified as Show

data Slot
    = Weapon
    deriving (Show.Show, Eq.Eq, Read.Read)

data Item = Item
    { name :: String.String
    , description :: String.String
    , slot :: Slot
    , bonus :: Int.Int32
    }
    deriving (Show.Show, Eq.Eq, Read.Read)

data Inventory = Inventory {gold :: Int.Int32, items :: [Item]} deriving (Show.Show, Eq.Eq, Read.Read)

totalSlotBonus :: Slot -> Inventory -> Int.Int32
totalSlotBonus s = items >>> Data.List.filter (slot >>> (== s)) >>> Functor.fmap bonus >>> Data.List.sum

totalDamageBonus :: Inventory -> Int.Int32
totalDamageBonus = totalSlotBonus Weapon

instance Semigroup.Semigroup Inventory where
    (Inventory g1 i1) <> (Inventory g2 i2) = Inventory (g1 + g2) (i1 <> i2)

instance Monoid.Monoid Inventory where
    mempty = Inventory 0 []

instance PrettyPrint.PrettyPrint Item where
    prettyPrint (Item name description _ bonus) =
        Text.Printf.printf
            "%s +%d\n\
            \%s"
            name
            bonus
            description

instance PrettyPrint.PrettyPrint [Item] where
    prettyPrint [] = ""
    prettyPrint items = Data.List.intercalate "\n\n" $ PrettyPrint.prettyPrint <$> items

instance PrettyPrint.PrettyPrint Inventory where
    prettyPrint (Inventory gold items) =
        Text.Printf.printf
            "Золото: %d\n\n"
            gold
            <> PrettyPrint.prettyPrint items
