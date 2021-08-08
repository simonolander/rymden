module Rymden.Helper.Foldable where

import Prelude
import Data.Foldable (class Foldable, foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple)

count :: forall f a. Foldable f => Ord a => f a -> Map a Int
count foldable = foldl (\map a -> Map.insertWith (+) a 1 map) Map.empty foldable

mapmap ::
  forall f g a b.
  Functor f =>
  Functor g =>
  (a -> b) -> f (g a) -> f (g b)
mapmap = (<$>) <<< (<$>)

infix 2 mapmap as <$$>

mapmapmap ::
  forall f g h a b.
  Functor f =>
  Functor g =>
  Functor h =>
  (a -> b) -> f (g (h a)) -> f (g (h b))
mapmapmap = (<$$>) <<< (<$>)

infix 2 mapmapmap as <$$$>
