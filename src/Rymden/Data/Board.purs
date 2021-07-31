module Rymden.Data.Board where

import Prelude
import Rymden.Data.BorderSegment (BorderSegment)
import Rymden.Data.Position (Position)
import Effect (Effect)

type Board =
  { width :: Int
  , height :: Int
  , borderSegments :: Array BorderSegment
  , centers :: Array Position
  }

empty :: Int -> Int -> Board
empty width height =
  { width, height, borderSegments: [], centers: [] }

generate :: Int -> Int -> Effect Board
generate width height = pure $ empty width height
