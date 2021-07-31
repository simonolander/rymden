module Rymden.Helper.Random where

import Prelude
import Effect (Effect)
import Effect.Random (random, randomInt)
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe)
import Data.Array as Array
import Data.Array ((!!))

randomBoolean :: Number -> Effect Boolean
randomBoolean probability = (_ < probability) <$> random

chooseOne :: forall f a. Foldable f => f a -> Effect (Maybe a)
chooseOne foldable =
  let
    array = Array.fromFoldable foldable

    maxIndex = Array.length array - 1
  in
    do
      index <- randomInt 0 maxIndex
      pure $ array !! index
