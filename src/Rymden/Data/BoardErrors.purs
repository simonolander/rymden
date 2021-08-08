module Rymden.Data.BoardErrors where

import Rymden.Data.BorderSegment (BorderSegment)
import Data.Set (Set)
import Rymden.Data.Position (Position)

type BoardErrors
  = { danglingBorders :: Set BorderSegment
    , incorrectGalaxySizes :: Set Position
    }
