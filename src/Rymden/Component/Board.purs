module Rymden.Component.Board where

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
import Rymden.Content.Campaigns (campaigns)
import Halogen.Store.Monad (class MonadStore)
import Rymden.Data.Store (Store)
import Rymden.Data.Progress (Progress)
import Halogen.Store.Connect (Connected)
import Halogen.Store.Connect (connect)
import Halogen.Store.Select (selectAll)
import Data.Maybe (Maybe(..))
import Halogen.Store.Select (selectEq)
import Rymden.Data.Progress (isLevelCompleted)
import Rymden.Data.Board (Board)
import Rymden.Data.Board as Board

type State
  = { board :: Maybe Board }

type Input
  = Unit

type StoreInput
  = Connected Progress Input

data Action
  = Receive StoreInput
  | Initialize

component ::
  forall query output storeAction m.
  MonadEffect m =>
  MonadStore storeAction Store m =>
  H.Component query Input output m
component = connect (selectEq _.progress) $ H.mkComponent { initialState, render, eval }
  where
  initialState :: StoreInput -> State
  initialState { context, input } = { board: Nothing }

  render :: forall slots. State -> HH.HTML (H.ComponentSlot slots m Action) Action
  render state = HH.text ""

  eval :: forall slots. H.HalogenQ query Action StoreInput ~> H.HalogenM State Action slots output m
  eval =
    H.mkEval
      $ H.defaultEval
          { handleAction = handleAction
          , receive = Just <<< Receive
          , initialize = Just Initialize
          }
    where
    handleAction = case _ of
      Receive input -> H.put $ initialState input
      Initialize -> do
        board <- H.liftEffect $ Board.generate 5 5
        H.modify_ _ { board = Just board }
