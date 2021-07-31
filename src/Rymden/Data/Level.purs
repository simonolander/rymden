module Rymden.Data.Level where

import Prelude

type LevelId
  = String

type Level
  = { id :: LevelId
    , name :: String
    }
