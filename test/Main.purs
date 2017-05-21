module Test.Main
  ( main
  ) where

import Control.Monad.Eff (Eff)
import Stuff
import Test.Pepper.Event as Pepper.Event
import Test.Spec.Reporter.Tap (tapReporter)
import Test.Spec.Runner (RunnerEffects, run)

main :: Eff (RunnerEffects ()) Unit
main = run [tapReporter] do
  Pepper.Event.spec
