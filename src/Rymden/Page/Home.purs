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

type State
  = Unit

type Action
  = Void

component ::
  forall query input output m.
  MonadEffect m =>
  H.Component query input output m
component = H.mkComponent { initialState, render, eval }
  where
  initialState :: input -> State
  initialState = const unit

  render :: forall slots. State -> HH.HTML (H.ComponentSlot slots m Action) Action
  render state =
    HH.div_
      [ HH.section
          [ classes "hero" ]
          [ HH.div
              [ classes "hero-body has-text-centered" ]
              [ HH.h1
                  [ classes "title" ]
                  [ HH.text "Rymden" ]
              , HH.h2
                  [ classes "subtitle" ]
                  [ HH.text "A functioning game" ]
              ]
          ]
      , HH.div
          [ classes "section" ]
          [ HH.div
              [ classes "container" ]
              [ HH.div
                  [ classes "buttons are-large" ]
                  [ HH.a [ classes "button is-primary is-fullwidth", href Play ] [ HH.text "Play" ]
                  , HH.a [ classes "button is-info is-outlined is-fullwidth", href Settings ] [ HH.text "Settings" ]
                  , HH.a [ classes "button is-link is-inverted is-fullwidth", HP.href "https://github.com/simonolander/rymden" ] [ HH.text "Source code" ]
                  ]
              ]
          ]
      ]

  eval :: forall slots. H.HalogenQ query Action input ~> H.HalogenM State Action slots output m
  eval = H.mkEval $ H.defaultEval
