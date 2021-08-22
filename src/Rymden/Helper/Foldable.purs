module Rymden.Helper.Foldable where

import Prelude
import Data.Foldable (class Foldable, foldl)
import Data.Map (Map)
import Data.Set (Set)
import Data.Set as Set
import Data.Map as Map
import Data.Tuple (Tuple)
import Data.Bifunctor (rmap)
import Data.Tuple (swap)

count :: forall f a. Foldable f => Ord a => f a -> Map a Int
count foldable = foldl (\map a -> Map.insertWith (+) a 1 map) Map.empty foldable

mapmap
  :: forall f g a b
   . Functor f
  => Functor g
  => (a -> b)
  -> f (g a)
  -> f (g b)
mapmap = (<$>) <<< (<$>)

infix 2 mapmap as <$$>

mapmapmap
  :: forall f g h a b
   . Functor f
  => Functor g
  => Functor h
  => (a -> b)
  -> f (g (h a))
  -> f (g (h b))
mapmapmap = (<$$>) <<< (<$>)

infix 2 mapmapmap as <$$$>

inverseMap :: forall k v. Ord k => Ord v => Map k v -> Map v (Set k)
inverseMap map =
  let
    tuples :: Array (Tuple k v)
    tuples = Map.toUnfoldableUnordered map
  in
    tuples
      <#> swap
      <#> rmap Set.singleton
      # Map.fromFoldableWith Set.union
