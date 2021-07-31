module Rymden.Page.Settings where

import Prelude
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Rymden.Component.Helpers.Property (classes)
import Halogen.Store.Monad (class MonadStore)
import Rymden.Data.Store (Store)
import Rymden.Data.Store as Store

type State
  = { confirmClear :: Boolean }

data Action
  = ClickedClear
  | ClickedConfirmClear
  | ClickedCancelClear

component ::
  forall query input output m.
  MonadEffect m =>
  MonadStore Store.Action Store m =>
  H.Component query input output m
component = H.mkComponent { initialState, render, eval }
  where
  initialState :: input -> State
  initialState = const { confirmClear: false }

  render :: forall slots. State -> HH.HTML (H.ComponentSlot slots m Action) Action
  render state =
    HH.div
      [ classes "section" ]
      [ HH.div
          [ classes "container" ]
          [ HH.h1 [ classes "title" ] [ HH.text "Settings" ]
          , if state.confirmClear then
              HH.div_
                [ HH.p [] [ HH.text "Are you sure? All data will be removed." ]
                , HH.button [ classes "button is-info is-outlined", HE.onClick $ const ClickedCancelClear ] [ HH.text "Cancel" ]
                , HH.button [ classes "button is-danger" ] [ HH.text "Yes, clear data" ]
                ]
            else
              HH.button [ classes "button is-danger", HE.onClick $ const ClickedClear ] [ HH.text "Clear data" ]
          ]
      ]

  eval :: forall slots. H.HalogenQ query Action input ~> H.HalogenM State Action slots output m
  eval =
    H.mkEval
      $ H.defaultEval
          { handleAction = handleAction }
    where
    handleAction = case _ of
      ClickedClear -> H.modify_ _ { confirmClear = true }
      ClickedCancelClear -> H.modify_ _ { confirmClear = false }
      ClickedConfirmClear -> pure unit
