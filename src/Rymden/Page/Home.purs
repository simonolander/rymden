module Rymden.Page.Home where

import Prelude
import Data.Const (Const)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Rymden.Component.Board as Board
import Rymden.Data.Store (Store)
import Rymden.Data.WindowProperties (WindowProperties)
import Type.Proxy (Proxy(..))
import Data.Int (toNumber)

type State
  = { solved :: Boolean
    , window :: WindowProperties
    }

data Action
  = ReceiveBoardOutput Board.Output

type Input
  = Unit

type StoreInput
  = Connected WindowProperties Input

type Slots
  = ( board :: H.Slot (Const Void) Board.Output Unit
    )

component ::
  forall query output storeAction m.
  MonadEffect m =>
  MonadStore storeAction Store m =>
  H.Component query Input output m
component = connect (selectEq _.window) $ H.mkComponent { initialState, render, eval }
  where
  initialState :: StoreInput -> State
  initialState { context } =
    { solved: false
    , window: context
    }

  render :: State -> HH.HTML (H.ComponentSlot Slots m Action) Action
  render state =
    HH.div
      []
      [ HH.slot (Proxy :: _ "board") unit Board.component boardInput ReceiveBoardOutput
      , HH.div_
          [ HH.button [] [ HH.text "Verify" ]
          ]
      ]
    where
    boardMaxWidth :: Number
    boardMaxWidth = toNumber state.window.width

    boardMaxHeight :: Number
    boardMaxHeight = toNumber state.window.height

    boardInput :: Board.Input
    boardInput =
      { maxWidth: boardMaxWidth
      , maxHeight: boardMaxHeight
      }

  eval :: H.HalogenQ query Action StoreInput ~> H.HalogenM State Action Slots output m
  eval =
    H.mkEval
      $ H.defaultEval
          { handleAction = handleAction }
    where
    handleAction :: Action -> H.HalogenM State Action Slots output m Unit
    handleAction = case _ of
      ReceiveBoardOutput (Board.Solved solved) -> H.modify_ _ { solved = solved }
