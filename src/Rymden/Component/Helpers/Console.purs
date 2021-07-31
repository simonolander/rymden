module Rymden.Component.Helpers.Console where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (error, errorShow)

hushError :: forall e a. Show e => String -> Either e a -> Effect (Maybe a)
hushError context e = case e of
  Left err -> do
    error context
    errorShow err
    pure Nothing
  Right a -> pure $ Just a
