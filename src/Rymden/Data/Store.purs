module Rymden.Data.Store where

import Prelude

import Rymden.Data.Settings (Settings)

type Store
  = { settings :: Settings
    }

data Action
  = Never

reduce :: Store -> Action -> Store
reduce store action = case action of
  _ -> store
