module Rymden.Data.BorderSegment where

import Prelude
import Data.Tuple (Tuple)
import Rymden.Data.Position (Position)

type BorderSegment
  = Tuple Position Position
