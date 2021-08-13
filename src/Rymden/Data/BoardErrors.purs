module Rymden.Data.BoardErrors where

import Data.Set (Set)
import Data.Set as Set
import Rymden.Data.BorderSegment (BorderSegment)
import Rymden.Data.Galaxy (Galaxy)
import Rymden.Data.Position (Position)
import Data.HeytingAlgebra (not, (||))

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

hasErrors :: BoardErrors -> Boolean
hasErrors boardErrors =
  not Set.isEmpty boardErrors.asymmetricCenters
    || not Set.isEmpty boardErrors.cellsInComponentsWithoutCenter
    || not Set.isEmpty boardErrors.danglingBorders
    || not Set.isEmpty boardErrors.incorrectGalaxySizes
