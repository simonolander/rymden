module Rymden.Data.Store where

import Prelude

import Rymden.Data.Settings (Settings)

type Store
  =
  { settings :: Settings
  }

type Action = Void

reduce :: Store -> Action -> Store
reduce store _ = store
