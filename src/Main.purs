module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Rymden.AppM (runAppM)
import Rymden.Component.Router as Router
import Rymden.Data.Route as Route
import Rymden.Data.Store (Store)
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)
import Rymden.Capability.ManageSettings (loadSettingsFromLocalStorage)
import Web.HTML as Web.HTML
import Web.HTML.Window as Window
import Effect.Console (info)
import Rymden.Meta.Version (version)
import Rymden.Meta.BuildTime (buildTime)

main :: Effect Unit
main = do
  info $ "Build info: \nVersion: " <> version <> "\nBuild time: " <> buildTime
  HA.runHalogenAff do
    settings <- H.liftEffect loadSettingsFromLocalStorage
    let
      initialStore :: Store
      initialStore = { settings }
    rootComponent <- runAppM initialStore Router.component
    body <- HA.awaitBody
    halogenIO <- runUI rootComponent unit body
    void $ H.liftEffect
      $ matchesWith (parse Route.route) \old new ->
        void
          $ when (old /= Just new) do
            launchAff_ $ halogenIO.query $ H.mkTell $ Router.Navigate new
