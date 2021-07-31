module Rymden.Data.Galaxy where

import Prelude
import Rymden.Data.BorderSegment (BorderSegment)
import Rymden.Data.Position (Position)

type Galaxy =
  { center :: Position
  , border :: Array BorderSegment
  }
