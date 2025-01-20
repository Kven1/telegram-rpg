module ScanWriter (ScanWriter, runM) where

import Control.Category ((>>>))
import Control.Monad.State qualified as State
import Control.Monad.Writer qualified as Writer
import Data.Functor.Identity qualified as Identity

type ScanWriter w a = State.StateT a (Writer.Writer w) a

runM :: ScanWriter w a -> a -> ((a, a), w)
runM = State.runStateT >>> (>>> Writer.runWriter)
