module Rymden.Data.BoardErrors where

import Rymden.Data.BorderSegment (BorderSegment)
import Data.Set (Set)

type BoardErrors
  = { danglingBorders :: Set BorderSegment
    }
