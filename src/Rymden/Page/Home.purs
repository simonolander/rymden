module Rymden.Page.Home where

import Prelude
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Rymden.Component.Board as Board
import Rymden.Data.Store (Store)
import Type.Proxy (Proxy(..))
import Data.Int (toNumber)
import Rymden.Component.Helpers.Property (classes)

type State
  =
  { solved :: Boolean
  }

data Action
  = ReceiveBoardOutput Board.Output
  | ClickedVerify
  | ClickedUndo
  | ClickedRedo
  | ClickedNew

type Input
  = Unit

type Slots
  =
  ( board :: H.Slot Board.Query Board.Output Unit
  )

component
  :: forall query output storeAction m
   . MonadEffect m
  => MonadStore storeAction Store m
  => H.Component query Input output m
component = H.mkComponent { initialState, render, eval }
  where
  initialState :: Input -> State
  initialState _ =
    { solved: false
    }

  render :: State -> HH.HTML (H.ComponentSlot Slots m Action) Action
  render state =
    HH.div
      [ classes "board-container" ]
      [ HH.slot _board unit Board.component unit ReceiveBoardOutput
      , renderButtons
      ]
    where
    renderButtons =
      if state.solved then
        HH.div
          [ classes "board-control-buttons" ]
          [ HH.button [ classes "board-control-button", HE.onClick $ const ClickedNew ] [ HH.text "New game" ]
          ]
      else
        HH.div
          [ classes "board-control-buttons" ]
          [ HH.button [ classes "board-control-button", HE.onClick $ const ClickedVerify ] [ HH.text "Display errors" ]
          --                  , HH.button [ classes "board-control-button", HE.onClick $ const ClickedUndo ] [ HH.text "Undo" ]
          --                  , HH.button [ classes "board-control-button", HE.onClick $ const ClickedRedo ] [ HH.text "Redo" ]
          ]

  eval :: H.HalogenQ query Action Input ~> H.HalogenM State Action Slots output m
  eval =
    H.mkEval
      $ H.defaultEval
        { handleAction = handleAction }
    where
    handleAction :: Action -> H.HalogenM State Action Slots output m Unit
    handleAction = case _ of
      ReceiveBoardOutput (Board.Solved solved) -> H.modify_ _ { solved = solved }
      ClickedVerify -> H.tell _board unit $ Board.HighlightErrors true
      ClickedUndo -> H.tell _board unit Board.Undo
      ClickedRedo -> H.tell _board unit Board.Redo
      ClickedNew -> do
        H.modify_ _ { solved = false }
        H.tell _board unit Board.New

  _board = (Proxy :: _ "board")
