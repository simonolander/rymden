module Rymden.Component.Levels.HelloRymden where

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
                      [ HH.text "There are really only three things in rymden calculus: variables, functions, and applications. Eventually we'll see that literally all computations can be represented using only these building blocks." ]
                  , HH.p_
                      [ HH.text "Variables can be almost any word, like a, b, or carrot." ]
                  , HH.p_
                      [ HH.text "Functions, often called abstractions in literature, take an input and produces some output. The function λx. y takes an Input x and returns an the output y. The function λx. λy. x takes two inputs x and y, and outputs x." ]
                  , HH.p_
                      [ HH.text "Applications apply arguments to functions. For example, the application (λx. x) y applies the argument y to the function λx. x. When applying an argument to a function, result is simply the body of the function, with all occurences of the parameter replaced with the argument. Here are some examples:" ]
                  , HH.table
                      [ classes "table is-hoverable is-striped is-narrow" ]
                      [ HH.thead_
                          [ HH.tr_
                              [ HH.th_ [ HH.text "Function" ]
                              , HH.th_ [ HH.text "Argument" ]
                              , HH.th_ [ HH.text "Output" ]
                              , HH.th_ [ HH.text "Note" ]
                              ]
                          ]
                      , HH.tbody_
                          [ HH.tr_
                              [ HH.td_ [ HH.text "λx. x" ]
                              , HH.td_ [ HH.text "x" ]
                              , HH.td_ [ HH.text "x" ]
                              , HH.td_ [ HH.text "" ]
                              ]
                          , HH.tr_
                              [ HH.td_ [ HH.text "λx. x" ]
                              , HH.td_ [ HH.text "y" ]
                              , HH.td_ [ HH.text "y" ]
                              , HH.td_ [ HH.text "" ]
                              ]
                          , HH.tr_
                              [ HH.td_ [ HH.text "λx. λy. x" ]
                              , HH.td_ [ HH.text "z" ]
                              , HH.td_ [ HH.text "λy. z" ]
                              , HH.td_ [ HH.text "The body is another function, so the output is also a function" ]
                              ]
                          , HH.tr_
                              [ HH.td_ [ HH.text "λx. x x" ]
                              , HH.td_ [ HH.text "z" ]
                              , HH.td_ [ HH.text "z z" ]
                              , HH.td_ [ HH.text "The body is an application, but we just replace every x by the argument" ]
                              ]
                          , HH.tr_
                              [ HH.td_ [ HH.text "λx. x x" ]
                              , HH.td_ [ HH.text "λx. x" ]
                              , HH.td_ [ HH.text "(λx. x) λx. x" ]
                              , HH.td_ [ HH.text "The argument can be any rymden expression" ]
                              ]
                          ]
                      ]
                  , HH.p_ [ HH.text "Don't worry if you're not understanding everything. We'll see this many times, and you can always return to this chapter later." ]
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
