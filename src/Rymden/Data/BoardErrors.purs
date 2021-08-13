module Rymden.Data.BoardErrors where

import Data.Set (Set)
import Data.Set as Set
import Rymden.Data.BorderSegment (BorderSegment)
import Rymden.Data.Galaxy (Galaxy)
import Rymden.Data.Position (Position)

type BoardErrors
  = { danglingBorders :: Set BorderSegment
    , incorrectGalaxySizes :: Set Position
    , cellsInComponentsWithoutCenter :: Set Position
    , asymmetricCenters :: Set Position
    }

empty :: BoardErrors
empty =
  { asymmetricCenters: Set.empty
  , cellsInComponentsWithoutCenter: Set.empty
  , danglingBorders: Set.empty
  , incorrectGalaxySizes: Set.empty
  }
