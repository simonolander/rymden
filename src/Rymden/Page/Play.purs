module Rymden.Page.Play where

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

type State
  = { progress :: Progress }

type Input
  = Unit

type StoreInput
  = Connected Progress Input

data Action
  = Receive StoreInput

component ::
  forall query output storeAction m.
  MonadEffect m =>
  MonadStore storeAction Store m =>
  H.Component query Input output m
component = connect (selectEq _.progress) $ H.mkComponent { initialState, render, eval }
  where
  initialState :: StoreInput -> State
  initialState { context, input } = { progress: context }

  render :: forall slots. State -> HH.HTML (H.ComponentSlot slots m Action) Action
  render state =
    HH.div_
      [ HH.section
          [ classes "hero" ]
          [ HH.div
              [ classes "hero-body has-text-centered" ]
              [ HH.h1
                  [ classes "title" ]
                  [ HH.text "Play" ]
              ]
          ]
      , HH.div_ $ renderCampaign <$> campaigns
      ]
    where
    renderCampaign campaign =
      HH.div_
        [ HH.section
            [ classes "hero is-primary" ]
            [ HH.div
                [ classes "hero-body" ]
                [ HH.h1 [ classes "title" ]
                    [ HH.text campaign.name ]
                ]
            ]
        , HH.div [ classes "section buttons" ] $ renderLevel <$> campaign.levels
        ]
      where
      renderLevel level =
        if isLevelCompleted level.id state.progress then
          HH.a
            [ classes "button is-info is-outlined"
            , href (Level level.id)
            ]
            [ HH.span_ [ HH.text level.name ]
            , HH.span
                [ classes "icon is-small" ]
                [ HH.i
                    [ classes "fas fa-star" ]
                    []
                ]
            ]
        else
          HH.a
            [ classes "button is-info"
            , href (Level level.id)
            ]
            [ HH.span_ [ HH.text level.name ]
            , HH.span
                [ classes "icon is-small" ]
                [ HH.i
                    [ classes "far fa-star" ]
                    []
                ]
            ]

  eval :: forall slots. H.HalogenQ query Action StoreInput ~> H.HalogenM State Action slots output m
  eval =
    H.mkEval
      $ H.defaultEval
          { handleAction = handleAction
          , receive = Just <<< Receive
          }
    where
    handleAction = case _ of
      Receive input -> H.put $ initialState input
