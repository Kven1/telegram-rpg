module Domain.Combat.Test (tests) where

import Data.Function (($))
import Data.List qualified as List
import Domain.Character qualified as Character
import Domain.Character.Class qualified as Character.Class
import Domain.Character.Inventory qualified as Inventory
import Domain.Combat qualified as Combat
import Domain.Enemies qualified as Enemies
import Domain.Entity.Attacker qualified as Attacker
import Domain.Entity.Health qualified as Health
import Test.HUnit qualified as HUnit

tests :: HUnit.Test
tests =
    HUnit.TestList
        [ HUnit.TestLabel "testCombat" testPlayerWins
        , HUnit.TestLabel "testEnemiesWin" testEnemiesWin
        ]

character =
    Character.Character
        { Character.name = "Character"
        , Character.cClass = Character.Class.Fighter
        , Character.health = Health.Health 2 2
        , Character.inventory = Inventory.Inventory 0 [Inventory.Item "Sword" "A sword" Inventory.Weapon 1]
        }

enemies =
    Enemies.Enemies
        { Enemies.description = "Enemy"
        , Enemies.health = Health.Health 2 2
        , Enemies.damage = 1
        }

combatWherePlayerWins :: Combat.Combat Character.Character Enemies.Enemies
combatWherePlayerWins = do
    Attacker.attack character
    Attacker.attack enemies
    Attacker.attack character

combatWhereEnemiesWin :: Combat.Combat Character.Character Enemies.Enemies
combatWhereEnemiesWin = do
    Attacker.attack enemies
    Attacker.attack character
    Attacker.attack enemies

testPlayerWins :: HUnit.Test
testPlayerWins = HUnit.TestCase $ do
    HUnit.assertEqual
        "Should conclude in player win"
        Combat.Finished{Combat.winner = Combat.Character, Combat.character = character{Character.health = Health.Health 1 2}}
        (Combat.state combatWherePlayerWins Combat.InProgress{Combat.character = character, Combat.enemies = enemies})

testEnemiesWin :: HUnit.Test
testEnemiesWin = HUnit.TestCase $ do
    HUnit.assertEqual
        "Should conclude in enemies win"
        (Combat.Finished{Combat.winner = Combat.Enemies, Combat.character = character{Character.health = Health.Health 0 2}})
        (Combat.state combatWhereEnemiesWin Combat.InProgress{Combat.character = character, Combat.enemies = enemies})
