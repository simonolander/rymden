module Rymden.Component.Router where

import Prelude
import Data.Const (Const)
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Class (class MonadEffect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HA
import Halogen.Store.Monad (class MonadStore)
import Rymden.Capability.Navigate (class Navigate, navigate)
import Rymden.Capability.ManageProgress (class ManageProgress)
import Rymden.Component.Helpers.Property (classes, href)
import Rymden.Data.Route (Route(..))
import Rymden.Data.Route as Route
import Rymden.Data.Store (Store)
import Rymden.Data.Store as Store
import Rymden.Page.Home as Home
import Rymden.Page.Level as Level
import Rymden.Page.Play as Play
import Rymden.Page.Settings as Settings
import Routing.Duplex (parse)
import Routing.Hash (getHash)
import Type.Proxy (Proxy(..))
import Data.Array (snoc)

type State
  = { route :: Maybe Route }

data Query a
  = Navigate Route a

data Action
  = Initialize

type Slots
  = ( home :: H.Slot (Const Void) Void Unit
    , settings :: H.Slot (Const Void) Void Unit
    , play :: H.Slot (Const Void) Void Unit
    , level :: H.Slot (Const Void) Void Unit
    )

component ::
  forall input output m.
  MonadAff m =>
  MonadEffect m =>
  MonadStore Store.Action Store m =>
  Navigate m =>
  ManageProgress m =>
  H.Component Query input output m
component = H.mkComponent { initialState, render, eval }
  where
  initialState :: input -> State
  initialState = const { route: Nothing }

  render :: State -> HH.HTML (H.ComponentSlot Slots m Action) Action
  render state =
    HH.div
      []
      [ HH.nav
          [ classes "navbar" ]
          [ HH.div
              [ classes "navbar-brand" ]
              [ HH.a
                  [ classes "navbar-item"
                  , href Home
                  ]
                  [ HH.img
                      [ HP.src "https://snappygoat.com/b/c6ec782d202013e1df10cc9a1fc7e578d865966a" ]
                  , HH.p [ classes "subtitle ml-2" ] [ HH.text "Rymden" ]
                  ]
              ]
          , HH.div
              [ classes "navbar-menu" ]
              [ HH.div
                  [ classes "navbar-start" ]
                  [ HH.div
                      [ classes "navbar-item" ]
                      [ renderBreadcrumbs state.route
                      ]
                  ]
              ]
          ]
      , case state.route of
          Just Home -> HH.slot (Proxy :: _ "home") unit Home.component unit absurd
          Just Settings -> HH.slot (Proxy :: _ "settings") unit Settings.component unit absurd
          Just Play -> HH.slot (Proxy :: _ "play") unit Play.component unit absurd
          Just (Level levelId) -> HH.slot (Proxy :: _ "level") unit Level.component levelId absurd
          Nothing -> HH.text "404 - Not found"
      ]
    where
    renderBreadcrumbs maybeRoute =
      HH.nav
        [ classes "breadcrumb"
        , HA.label "breadcrumbs"
        ]
        [ HH.ul_ $ renderLinks maybeRoute
        ]
      where
      renderLinks route =
        let
          props = if route == maybeRoute then [ classes "is-active" ] else []
        in
          case route of
            Just Home -> [ HH.li props [ HH.a [ href Home ] [ HH.text (show Home) ] ] ]
            Just Settings -> renderLinks (Just Home) `snoc` HH.li props [ HH.a [ href Settings ] [ HH.text (show Settings) ] ]
            Just Play -> renderLinks (Just Home) `snoc` HH.li props [ HH.a [ href Play ] [ HH.text (show Play) ] ]
            Just (Level id) -> renderLinks (Just Play) `snoc` HH.li props [ HH.a [ href (Level id) ] [ HH.text id ] ]
            Nothing -> renderLinks (Just Home)

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
