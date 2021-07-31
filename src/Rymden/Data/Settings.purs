module Rymden.Data.Settings where

import Prelude

type Settings
  = { username :: String }

initialSettings :: Settings
initialSettings = { username: "" }
