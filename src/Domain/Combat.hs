module Domain.Combat (Combat, emitAction, Action (..), Side (..), history, State (..), state, mapCharacter) where

import Control.Applicative qualified as Applicative
import Control.Monad.State qualified as State
import Control.Monad.Writer qualified as Writer
import Data.Eq qualified as Eq
import Data.Function (($), (&))
import Data.Int qualified as Int
import Data.Tuple qualified as Tuple
import Domain.Character.Inventory qualified as Inventory
import Domain.Entity.Damageable qualified as Damageable
import Domain.Entity.Looter qualified as Looter
import ScanWriter qualified
import Text.Printf qualified as Print
import Text.Show qualified as Show

data Side = Character | Enemies deriving (Eq.Eq, Show.Show)
data Action
    = Attack {side :: Side, damage :: Int.Int32}
    | TakeLoot Inventory.Inventory
    deriving (Eq.Eq)

data State c e
    = InProgress {character :: (Damageable.Damageable c) => c, enemies :: (Damageable.Damageable e) => e}
    | Finished {character :: (Damageable.Damageable c) => c, winner :: Side}

mapCharacter :: (Damageable.Damageable c) => (c -> c') -> State c e -> State c' e
mapCharacter f (InProgress c e) = InProgress (f c) e
mapCharacter f (Finished c w) = Finished (f c) w

deriving instance (Eq.Eq c, Eq.Eq e, Damageable.Damageable c, Damageable.Damageable e) => Eq.Eq (State c e)
deriving instance (Show.Show c, Show.Show e, Damageable.Damageable c, Damageable.Damageable e) => Show.Show (State c e)

type Combat c e = ScanWriter.ScanWriter [Action] (State c e)

instance Show.Show Action where
    show action = Print.printf "Side: %s, Damage: %d" (Show.show $ side action) (damage action)

instance Show.Show (Combat c e) where
    show _ = "Combat"

history :: Combat c e -> State c e -> [Action]
history combat initialState = Tuple.snd $ ScanWriter.runM combat initialState

state :: Combat c e -> State c e -> State c e
state combat initialState = Tuple.snd $ Tuple.fst $ ScanWriter.runM combat initialState

emitAction :: (Damageable.Damageable c, Looter.Looter c, Damageable.Damageable e) => Action -> Combat c e
emitAction action = do
    currentState <- State.get
    let updatedState = applyAction currentState action
    State.put updatedState

    Writer.lift $ Writer.tell [action]

    Applicative.pure updatedState

applyAction :: (Damageable.Damageable c, Looter.Looter c, Damageable.Damageable e) => State c e -> Action -> State c e
applyAction state@InProgress{} (Attack Character damage) = state{enemies = Damageable.takeDamage (enemies state) damage} & checkForDeath Enemies
applyAction state@InProgress{} (Attack Enemies damage) = state{character = Damageable.takeDamage (character state) damage} & checkForDeath Character
applyAction state@(Finished c Character) (TakeLoot loot) = state{character = Looter.takeLoot loot c}
applyAction state@Finished{} (Attack _ _) = state

checkForDeath :: (Damageable.Damageable c, Damageable.Damageable e) => Side -> State c e -> State c e
checkForDeath Character state = if Damageable.isDead (character state) then Finished (character state) Enemies else state
checkForDeath Enemies state = if Damageable.isDead (enemies state) then Finished (character state) Character else state
