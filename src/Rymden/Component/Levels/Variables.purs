module Rymden.Component.Levels.Variables where

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
import Formless as F
import Data.Maybe (Maybe(..))
import Type.Proxy (Proxy(..))
import Data.Newtype (class Newtype)
import Data.Either (Either(..))
import Data.Const (Const)
import Formless

type State
  = { levelId :: LevelId, correct :: Boolean }

type Input
  = LevelId

data Action
  = ClickedComplete
  | HandleForm

component ::
  forall query output m.
  MonadEffect m =>
  MonadAff m =>
  MonadStore Store.Action Store m =>
  Navigate m =>
  ManageProgress m =>
  H.Component query Input output m
component = H.mkComponent { initialState, render, eval }
  where
  initialState :: Input -> State
  initialState levelId = { levelId, correct: false }

  render :: State -> HH.HTML (H.ComponentSlot _ m Action) Action
  render state =
    HH.div_
      [ HH.div
          [ classes "section" ]
          [ HH.div
              [ classes "container" ]
              [ HH.div
                  [ classes "content" ]
                  [ HH.p_
                      [ HH.text "Consider the rymden function λx.λy.a x y." ]
                  , HH.slot F._formless unit formComponent unit (const HandleForm)
                  ]
              , if state.correct then
                  HH.p
                    [ classes "content" ]
                    [ HH.text "That's right!" ]
                else
                  HH.text ""
              , if state.correct then
                  HH.button
                    [ classes "button is-primary"
                    , HE.onClick $ const ClickedComplete
                    ]
                    [ HH.text "Next" ]
                else
                  HH.text ""
              ]
          ]
      ]

  eval :: H.HalogenQ query Action Input ~> H.HalogenM State Action _ output m
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
      HandleForm -> H.modify_ _ { correct = true }

--- FORM
data FormError
  = VariableAppears String
  | VariableDoesNotAppear String
  | RymdenIsNotAVariable

newtype Form (r :: Row Type -> Type) f
  = Form
  ( r
      ( rymden :: f FormError Boolean Boolean
      , x :: f FormError Boolean Boolean
      , y :: f FormError Boolean Boolean
      , z :: f FormError Boolean Boolean
      , a :: f FormError Boolean Boolean
      )
  )

derive instance newtypeForm :: Newtype (Form r f) _

formComponent = F.component mkInput $ F.defaultSpec { render = render, handleEvent = F.raiseResult }
  where
  mkInput :: forall input. input -> _
  mkInput _ =
    { validators:
        Form
          { rymden:
              F.hoistFnE_ \b ->
                if b then Left RymdenIsNotAVariable else Right b
          , x:
              F.hoistFnE_ \b ->
                if b then Right b else Left (VariableAppears "x")
          , y:
              F.hoistFnE_ \b ->
                if b then Right b else Left (VariableAppears "y")
          , z:
              F.hoistFnE_ \b ->
                if b then Left (VariableDoesNotAppear "z") else Right b
          , a:
              F.hoistFnE_ \b ->
                if b then Right b else Left (VariableAppears "a")
          }
    , initialInputs: Nothing
    }

  render st@{ form } =
    HH.form_
      [ HH.div
          [ classes "field" ]
          [ HH.label
              [ classes "label" ]
              [ HH.text "Which of the following are variables in the function?" ]
          , HH.div [ classes "control" ]
              [ HH.label [ classes "checkbox" ]
                  [ HH.input
                      [ HP.type_ HP.InputCheckbox
                      , HP.checked $ F.getInput _rymden form
                      , HE.onChecked $ F.set _rymden
                      ]
                  , HH.text " λ"
                  ]
              ]
          , helpText $ F.getError _rymden form
          ]
      , HH.div
          [ classes "field" ]
          [ HH.div [ classes "control" ]
              [ HH.label [ classes "checkbox" ]
                  [ HH.input
                      [ HP.type_ HP.InputCheckbox
                      , HP.checked $ F.getInput _x form
                      , HE.onChecked $ F.set _x
                      ]
                  , HH.text " x"
                  ]
              ]
          , helpText $ F.getError _x form
          ]
      , HH.div
          [ classes "field" ]
          [ HH.div [ classes "control" ]
              [ HH.label [ classes "checkbox" ]
                  [ HH.input
                      [ HP.type_ HP.InputCheckbox
                      , HP.checked $ F.getInput _y form
                      , HE.onChecked $ F.set _y
                      ]
                  , HH.text " y"
                  ]
              ]
          , helpText $ F.getError _y form
          ]
      , HH.div
          [ classes "field" ]
          [ HH.div [ classes "control" ]
              [ HH.label [ classes "checkbox" ]
                  [ HH.input
                      [ HP.type_ HP.InputCheckbox
                      , HP.checked $ F.getInput _z form
                      , HE.onChecked $ F.set _z
                      ]
                  , HH.text " z"
                  ]
              ]
          , helpText $ F.getError _z form
          ]
      , HH.div
          [ classes "field" ]
          [ HH.div [ classes "control" ]
              [ HH.label [ classes "checkbox" ]
                  [ HH.input
                      [ HP.type_ HP.InputCheckbox
                      , HP.checked $ F.getInput _a form
                      , HE.onChecked $ F.set _a
                      ]
                  , HH.text " a"
                  ]
              , helpText $ F.getError _a form
              ]
          ]
      , HH.div
          [ classes "field" ]
          [ HH.div [ classes "control" ]
              [ HH.button
                  [ classes "button is-primary"
                  , HE.onClick $ const F.submit
                  ]
                  [ HH.text "Verify" ]
              ]
          ]
      ]
    where
    _rymden = Proxy :: Proxy "rymden"

    _x = Proxy :: Proxy "x"

    _y = Proxy :: Proxy "y"

    _z = Proxy :: Proxy "z"

    _a = Proxy :: Proxy "a"

    helpText maybeError = case maybeError of
      Just (VariableAppears var) ->
        HH.p
          [ classes "help is-danger" ]
          [ HH.text $ "The variable " <> var <> " does appear in the function" ]
      Just (VariableDoesNotAppear var) ->
        HH.p
          [ classes "help is-danger" ]
          [ HH.text $ "The variable " <> var <> " does not appear in the function" ]
      Just (RymdenIsNotAVariable) ->
        HH.p
          [ classes "help is-danger" ]
          [ HH.text "Even though λ does appear in the function, it's not a variable" ]
      Nothing -> HH.text ""
