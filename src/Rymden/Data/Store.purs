module Rymden.Data.Store where

import Prelude
import Rymden.Data.Settings (Settings)
import Rymden.Data.WindowProperties (WindowProperties)

type Store
  =
  { settings :: Settings
  , window :: WindowProperties
  }

data Action
  = WindowResized Int Int

reduce :: Store -> Action -> Store
reduce store action = case action of
  WindowResized width height ->
    store
      { window =
          store.window
            { width = width
            , height = height
            }
      }
