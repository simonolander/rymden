module Rymden.Page.Level where

import Prelude
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Monad (class MonadStore)
import Rymden.Capability.ManageProgress (class ManageProgress)
import Rymden.Capability.Navigate (class Navigate)
import Rymden.Component.Helpers.Property (classes)
import Rymden.Component.Levels.BooleanValues as BooleanValues
import Rymden.Component.Levels.HelloRymden as HelloRymden
import Rymden.Component.Levels.Variables as Variables
import Rymden.Content.Campaigns (getLevel)
import Rymden.Data.Level (Level, LevelId)
import Rymden.Data.Store (Store)
import Rymden.Data.Store as Store
import Type.Proxy (Proxy(..))

type State
  = { levelId :: LevelId
    , level :: Maybe Level
    }

type Input
  = LevelId

data Action
  = ClickedComplete

type Slots
  = ( level :: H.Slot (Const Void) Void Unit )

component ::
  forall query output m.
  MonadAff m =>
  MonadEffect m =>
  MonadStore Store.Action Store m =>
  Navigate m =>
  ManageProgress m =>
  H.Component query Input output m
component = H.mkComponent { initialState, render, eval }
  where
  initialState :: Input -> State
  initialState levelId = { levelId, level: getLevel levelId }

  render :: State -> HH.HTML (H.ComponentSlot Slots m Action) Action
  render state = case state.level of
    Just level ->
      HH.div_
        [ HH.section
            [ classes "hero" ]
            [ HH.div
                [ classes "hero-body has-text-centered" ]
                [ HH.h1
                    [ classes "title" ]
                    [ HH.text level.name ]
                ]
            ]
        , case state.level <#> _.id of
            Just "helloworld" -> HH.slot (Proxy :: _ "level") unit HelloRymden.component state.levelId absurd
            Just "booleans" -> HH.slot (Proxy :: _ "level") unit BooleanValues.component state.levelId absurd
            Just "variables" -> HH.slot (Proxy :: _ "level") unit Variables.component state.levelId absurd
            _ -> HH.p_ [ HH.text "404 - Not found" ]
        ]
    Nothing -> HH.text "404 - Not found"

  eval :: H.HalogenQ query Action Input ~> H.HalogenM State Action Slots output m
  eval = H.mkEval $ H.defaultEval
