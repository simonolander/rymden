module Rymden.Capability.ManageSettings where

import Prelude
import Control.Monad.Reader.Trans (lift)
import Data.Argonaut.Core (fromString, stringify)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Halogen (HalogenM)
import Rymden.Data.Route (Route)
import Rymden.Data.Settings (Settings, initialSettings)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

class
  Monad m <=
  ManageSettings m where
  saveSettings :: Settings -> m Unit

instance navigateHalogenM :: ManageSettings m => ManageSettings (HalogenM state action slots output m) where
  saveSettings = lift <<< saveSettings

settingsKey :: String
settingsKey = "settings"

loadSettingsFromLocalStorage :: Effect Settings
loadSettingsFromLocalStorage = do
  storage <- localStorage =<< window
  maybeSettingsString <- getItem settingsKey storage
  pure $ fromMaybe initialSettings
    $ case maybeSettingsString of
      Just settingsString -> hush $ decodeJson $ fromString settingsString
      Nothing -> Nothing

saveSettingsToLocalStorage :: Settings -> Effect Unit
saveSettingsToLocalStorage settings = do
  let
    settingsString = stringify $ encodeJson settings
  storage <- localStorage =<< window
  setItem settingsKey settingsString storage
