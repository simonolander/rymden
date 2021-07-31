module Rymden.Page.Home where

import Prelude
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Rymden.Component.Helpers.Property (classes)
import Rymden.Data.Route (Route(..))
import Rymden.Component.Helpers.Property (href)
import Halogen.Svg.Elements as Svg
import Data.Const (Const)
import Type.Proxy (Proxy(..))
import Rymden.Component.Board as Board
import Halogen.Store.Monad (class MonadStore)
import Rymden.Data.Store (Store)

type State
  = Unit

type Action
  = Void

type Slots
  = ( board :: H.Slot (Const Void) Void Unit
    )

component ::
  forall query input output storeAction m.
  MonadEffect m =>
  MonadStore storeAction Store m =>
  H.Component query input output m
component = H.mkComponent { initialState, render, eval }
  where
  initialState :: input -> State
  initialState = const unit

  render :: State -> HH.HTML (H.ComponentSlot Slots m Action) Action
  render state =
    HH.slot (Proxy :: _ "board") unit Board.component unit absurd

  eval :: H.HalogenQ query Action input ~> H.HalogenM State Action Slots output m
  eval = H.mkEval $ H.defaultEval
