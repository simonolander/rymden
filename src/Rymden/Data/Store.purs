module Rymden.Data.Store where

import Prelude
import Rymden.Data.Settings (Settings)
import Rymden.Data.Progress (Progress)

type Store
  = { settings :: Settings
    , progress :: Progress
    }

data Action
  = ReplaceProgress Progress

reduce :: Store -> Action -> Store
reduce store action = case action of
  ReplaceProgress progress -> store { progress = progress }
