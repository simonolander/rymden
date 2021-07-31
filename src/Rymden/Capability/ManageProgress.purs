module Rymden.Capability.ManageProgress where

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
import Rymden.Data.Progress (Progress, initialProgress)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)
import Rymden.Data.Progress (Progress)
import Rymden.Data.Level (LevelId)
import Rymden.Component.Helpers.Console (hushError)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Effect.Console (error)

class
  Monad m <= ManageProgress m where
  saveProgress :: Progress -> m Unit
  completeLevel :: LevelId -> m Unit

instance navigateHalogenM :: ManageProgress m => ManageProgress (HalogenM state action slots output m) where
  saveProgress = lift <<< saveProgress
  completeLevel = lift <<< completeLevel

progressKey :: String
progressKey = "progress"

loadProgressFromLocalStorage :: Effect Progress
loadProgressFromLocalStorage = do
  storage <- localStorage =<< window
  maybeProgressString <- getItem progressKey storage
  maybeProgress <- case maybeProgressString of
    Just progressString -> case jsonParser progressString of
      Right json -> hushError "loadProgressFromLocalStorage" (decodeJson json)
      Left err -> do
        error err
        pure Nothing
    Nothing -> pure Nothing
  pure $ fromMaybe initialProgress maybeProgress

saveProgressToLocalStorage :: Progress -> Effect Unit
saveProgressToLocalStorage progress = do
  let
    progressString = stringify $ encodeJson progress
  storage <- localStorage =<< window
  setItem progressKey progressString storage
