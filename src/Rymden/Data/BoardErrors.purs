module Rymden.Data.BoardErrors where

import Rymden.Data.BorderSegment (BorderSegment)
import Data.Set (Set)
import Rymden.Data.Position (Position)
import Rymden.Data.Galaxy (Galaxy)

type BoardErrors
  = { danglingBorders :: Set BorderSegment
    , incorrectGalaxySizes :: Set Position
    , cellsInComponentsWithoutCenter :: Set Position
    , asymmetricCenters :: Set Position
    }
