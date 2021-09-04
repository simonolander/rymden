module Rymden.Component.Router where

import Prelude

import Data.Const (Const)
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Query.Event as HQ
import Halogen.Store.Monad (class MonadStore, updateStore)
import Routing.Duplex (parse)
import Routing.Hash (getHash)
import Rymden.Capability.Navigate (class Navigate, navigate)
import Rymden.Component.Helpers.Property (classes)
import Rymden.Data.Route (Route(..))
import Rymden.Data.Route as Route
import Rymden.Data.Store (Store)
import Rymden.Data.Store as Store
import Rymden.Page.Home as Home
import Rymden.Page.Settings as Settings
import Type.Proxy (Proxy(..))
import Web.Event.Event (EventType(..))
import Web.HTML as Web.HTML
import Web.HTML.Window as Window

type State
  = { route :: Maybe Route }

data Query a
  = Navigate Route a

data Action
  = Initialize

type Slots
  =
  ( home :: H.Slot (Const Void) Void Unit
  , settings :: H.Slot (Const Void) Void Unit
  )

component
  :: forall input output m
   . MonadAff m
  => MonadEffect m
  => MonadStore Store.Action Store m
  => Navigate m
  => H.Component Query input output m
component = H.mkComponent { initialState, render, eval }
  where
  initialState :: input -> State
  initialState = const { route: Nothing }

  render :: State -> HH.HTML (H.ComponentSlot Slots m Action) Action
  render state =
    HH.div
      [ classes "main" ]
      [ case state.route of
          Just Home -> HH.slot (Proxy :: _ "home") unit Home.component unit absurd
          Just Settings -> HH.slot (Proxy :: _ "settings") unit Settings.component unit absurd
          Nothing -> HH.text "404 - Not found"
      ]

  eval :: H.HalogenQ Query Action input ~> H.HalogenM State Action Slots output m
  eval =
    H.mkEval
      H.defaultEval
        { initialize = initialize
        , handleAction = handleAction
        , handleQuery = handleQuery
        }
    where
    initialize :: Maybe Action
    initialize = Just Initialize

    handleAction :: Action -> H.HalogenM State Action Slots output m Unit
    handleAction action = case action of
      Initialize -> do
        initialRoute <- H.liftEffect $ hush <<< (parse Route.route) <$> getHash
        navigate $ fromMaybe Home initialRoute

    handleQuery :: forall a. Query a -> H.HalogenM State Action Slots output m (Maybe a)
    handleQuery query = case query of
      Navigate newRoute a -> do
        { route } <- H.get
        when (route /= Just newRoute) do
          H.modify_ _ { route = Just newRoute }
        pure (Just a)
