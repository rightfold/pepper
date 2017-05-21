module Pepper.Event
  ( Event(..)
  , handle
  ) where

import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, (.?), decodeJson)
import Data.DateTime.Instant (Instant, instant)
import Stuff

--------------------------------------------------------------------------------

data Event (i :: Symbol) a = Event Instant a
derive instance ee :: (Eq a) => Eq (Event i a)
derive instance oe :: (Ord a) => Ord (Event i a)
derive instance fe :: Functor (Event i)
instance le :: Foldable (Event i) where
  foldMap f (Event _ a) = f a
  foldl f z (Event _ a) = f z a
  foldr f z (Event _ a) = f a z
instance te :: Traversable (Event i) where
  sequence (Event i a) = Event i <$> a
  traverse f (Event i a) = Event i <$> f a

--------------------------------------------------------------------------------

handle :: ∀ i a f. IsSymbol i => DecodeJson a => Applicative f =>
  (Event i a -> f Unit) -> Json -> f (String \/ Unit)
handle handler rawEvent =
  let identifier = reflectSymbol (SProxy :: SProxy i) in
  case decodeEvent rawEvent of
    Left err -> pure (Left err)
    Right (identifier' /\ event)
      | identifier' == identifier -> traverse handler \ traverse force $ event
      | otherwise -> pure \ pure $ unit

decodeEvent :: ∀ i a. DecodeJson a => Json ->
  String \/ (String /\ Event i (Lazy (String \/ a)))
decodeEvent json = do
  rawEvent <- decodeJson json
  identifier <- decodeIdentifier =<< rawEvent .? "identifier"
  timestamp <- decodeTimestamp =<< rawEvent .? "timestamp"
  value <- decodeValue <$> rawEvent .? "value"
  pure $ identifier /\ Event timestamp value

decodeIdentifier :: Json -> String \/ String
decodeIdentifier = decodeJson

decodeTimestamp :: Json -> String \/ Instant
decodeTimestamp =
  maybe (throwError "timestamp out of range") pure <=<
    map (instant <<< wrap) <<< decodeJson

decodeValue :: ∀ a. DecodeJson a => Json -> Lazy (String \/ a)
decodeValue json = defer \_ -> decodeJson json
