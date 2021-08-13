module Rymden.Page.Home where

import Prelude
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Data.Const (Const)
import Type.Proxy (Proxy(..))
import Rymden.Component.Board as Board
import Halogen.Store.Monad (class MonadStore)
import Rymden.Data.Store (Store)
import Debug (spy)

type State
  = { solved :: Boolean
    }

data Action
  = ReceiveBoardOutput Board.Output

type Slots
  = ( board :: H.Slot (Const Void) Board.Output Unit
    )

component ::
  forall query input output storeAction m.
  MonadEffect m =>
  MonadStore storeAction Store m =>
  H.Component query input output m
component = H.mkComponent { initialState, render, eval }
  where
  initialState :: input -> State
  initialState = const { solved: false }

  render :: State -> HH.HTML (H.ComponentSlot Slots m Action) Action
  render _state =
    HH.div
      []
      [ HH.slot (Proxy :: _ "board") unit Board.component unit ReceiveBoardOutput
      , HH.div_
          [ HH.button [] [ HH.text "Verify" ]
          ]
      ]

  eval :: H.HalogenQ query Action input ~> H.HalogenM State Action Slots output m
  eval =
    H.mkEval
      $ H.defaultEval
          { handleAction = handleAction }
    where
    handleAction :: Action -> H.HalogenM State Action Slots output m Unit
    handleAction = case _ of
      ReceiveBoardOutput (Board.Solved solved) -> H.modify_ _ { solved = spy "solved" solved }
