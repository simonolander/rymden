module Rymden.Data.GalaxyCenter where

import Prelude
import Rymden.Data.Position (Position)

type GalaxyCenter
  = { position :: Position
    , galaxySize :: Int
    }
