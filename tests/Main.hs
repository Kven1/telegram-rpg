module Main where

import Data.Ord ((>))
import Domain.Combat.Test qualified as Combat.Test
import System.Exit qualified as Exit
import System.IO qualified as IO
import Test.HUnit qualified as HUnit

tests :: HUnit.Test
tests = Combat.Test.tests

main :: IO.IO ()
main = do
    result <- HUnit.runTestTT tests
    if HUnit.failures result > 0 then Exit.exitFailure else Exit.exitSuccess
