module Rymden.Component.Levels.BooleanValues where

import Prelude
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore, getStore, updateStore)
import Rymden.Capability.Navigate (class Navigate, navigate)
import Rymden.Capability.ManageProgress (class ManageProgress, completeLevel)
import Rymden.Component.Helpers.Property (classes, href)
import Rymden.Data.Level (LevelId)
import Rymden.Data.Progress (isLevelCompleted)
import Rymden.Data.Route (Route(..))
import Rymden.Data.Store (Store)
import Rymden.Data.Store as Store

type State
  = { levelId :: LevelId }

type Input
  = LevelId

data Action
  = ClickedComplete

component ::
  forall query output m.
  MonadEffect m =>
  MonadStore Store.Action Store m =>
  Navigate m =>
  ManageProgress m =>
  H.Component query Input output m
component = H.mkComponent { initialState, render, eval }
  where
  initialState :: Input -> State
  initialState levelId = { levelId }

  render :: forall slots. State -> HH.HTML (H.ComponentSlot slots m Action) Action
  render state =
    HH.div_
      [ HH.div
          [ classes "section" ]
          [ HH.div
              [ classes "container" ]
              [ HH.div
                  [ classes "content" ]
                  [ HH.p_
                      [ HH.text "The most fundamental data type in computer science is the boolean: true or false. If you've done any kind of programming before, you've seen them. Here's an example in Javascript:" ]
                  , HH.pre_
                      [ HH.code_
                          [ HH.text "if (x) {\n  return a\n} else {\n  return b\n}" ]
                      ]
                  , HH.p_
                      [ HH.text "In the sample above, we have a boolean x, and depending on if it's true or false, we return a or b respectively. In a sense, booleans lets us choose between two outcomes." ]
                  , HH.p_
                      [ HH.text "Consider the rymden function λa.λb.a. It takes two values, and outputs the first value you provided it. It sort of behaves like the snippet above, in the case that x is true, doesn't it?" ]
                  , HH.p_
                      [ HH.text "Similarily, the function λa.λb.b takes two values, discards the first value and outputs the second value you provided." ]
                  , HH.p_
                      [ HH.text "These two functions will be our representation of true and false!" ]
                  , HH.table
                      [ classes "table is-hoverable is-striped is-narrow" ]
                      [ HH.thead_
                          [ HH.tr_
                              [ HH.th_ [ HH.text "Name" ]
                              , HH.th_ [ HH.text "λ-function" ]
                              ]
                          ]
                      , HH.tbody_
                          [ HH.tr_
                              [ HH.td_ [ HH.text "True" ]
                              , HH.td_ [ HH.text "λa. λb. a" ]
                              ]
                          , HH.tr_
                              [ HH.td_ [ HH.text "False" ]
                              , HH.td_ [ HH.text "λa. λb. b" ]
                              ]
                          ]
                      ]
                  , HH.button
                      [ classes "button is-primary"
                      , HE.onClick $ const ClickedComplete
                      ]
                      [ HH.text "Okay!" ]
                  ]
              ]
          ]
      ]

  eval :: forall slots. H.HalogenQ query Action Input ~> H.HalogenM State Action slots output m
  eval =
    H.mkEval
      $ H.defaultEval
          { handleAction = handleAction }
    where
    handleAction = case _ of
      ClickedComplete -> do
        levelId <- H.gets _.levelId
        completeLevel levelId
        navigate Play
