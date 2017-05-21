module Test.Pepper.Event
  ( spec
  ) where

import Control.Monad.Writer.Class as Writer
import Control.Monad.Writer.Trans (runWriterT)
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Encode (class EncodeJson, (~>), (:=))
import Data.DateTime.Instant (Instant, unInstant)
import Pepper.Event (Event(..), handle)
import Stuff
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

rawEvent :: ∀ a. EncodeJson a => String -> Instant -> a -> Json
rawEvent identifier timestamp value =
  "identifier" := identifier
  ~> "timestamp" := unwrap (unInstant timestamp)
  ~> "value" := value
  ~> jsonEmptyObject

spec :: ∀ r. Spec r Unit
spec = describe "Pepper.Event" do
  describe "handle" do
    it "invokes the matching handler exactly one time" do
      result /\ log <- runWriterT $
        handle (\(Event _ n :: Event "increment" Int) -> Writer.tell [n]) $
          rawEvent "increment" bottom 1
      result `shouldEqual` pure unit
      log `shouldEqual` [1]
    it "invokes the mismatching handler exactly zero times" do
      result /\ log <- runWriterT $
        handle (\(Event _ n :: Event "increment" Int) -> Writer.tell [n]) $
          rawEvent "decrement" bottom 1
      result `shouldEqual` pure unit
      log `shouldEqual` []
