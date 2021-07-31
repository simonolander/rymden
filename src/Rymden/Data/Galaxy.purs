module Rymden.Data.Galaxy where

import Prelude
import Rymden.Data.BorderSegment (BorderSegment)
import Rymden.Data.Position (Position)
import Data.Set (Set)

type Galaxy
  = Set Position

type GalaxyArm
  = { p1 :: Position
    , p2 :: Position
    }
