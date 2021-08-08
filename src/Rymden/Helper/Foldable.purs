module Rymden.Helper.Foldable where

import Prelude

import Data.Foldable (class Foldable, foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple)

count :: forall f a. Foldable f => Ord a => f a -> Map a Int
count foldable =
  foldl (\ map a -> Map.insertWith (+) a 1 map) Map.empty foldable

