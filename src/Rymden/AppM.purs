module Rymden.AppM where

import Prelude
import Control.Monad.Reader.Class (asks)
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, runReaderT)
import Control.Parallel (class Parallel, parallel, sequential)
import Effect.Aff (Aff, ParAff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.Store.Monad (runStoreT)
import Rymden.Capability.Navigate (class Navigate)
import Rymden.Capability.ManageSettings (class ManageSettings)
import Rymden.Capability.ManageProgress (class ManageProgress)
import Rymden.Data.Route as Route
import Rymden.Data.Store (Store)
import Rymden.Data.Store as Store
import Routing.Duplex (print)
import Routing.Hash (setHash)
import Type.Equality (class TypeEquals, from)
import Halogen.Store.Monad (StoreT)
import Safe.Coerce (coerce)
import Halogen as H
import Halogen.Store.Monad (class MonadStore, getStore, updateStore)
import Rymden.Capability.ManageSettings (saveSettingsToLocalStorage)
import Rymden.Capability.ManageProgress (saveProgressToLocalStorage, saveProgress)
import Rymden.Data.Progress (isLevelCompleted)
import Rymden.Data.Store (Action(..))
import Rymden.Data.Progress (withCompleteLevel)

newtype AppM a
  = AppM (StoreT Store.Action Store Aff a)

runAppM :: forall query input output. Store.Store -> H.Component query input output AppM -> Aff (H.Component query input output Aff)
runAppM store = runStoreT store Store.reduce <<< coerce

derive newtype instance functorAppM :: Functor AppM

derive newtype instance applyAppM :: Apply AppM

derive newtype instance applicativeAppM :: Applicative AppM

derive newtype instance bindAppM :: Bind AppM

derive newtype instance monadAppM :: Monad AppM

derive newtype instance monadEffectAppM :: MonadEffect AppM

derive newtype instance monadAffAppM :: MonadAff AppM

derive newtype instance monadStoreAppM :: MonadStore Store.Action Store AppM

instance navigateAppM :: Navigate AppM where
  navigate = liftEffect <<< setHash <<< print Route.route

instance manageSettingsAppM :: ManageSettings AppM where
  saveSettings settings = liftEffect $ saveSettingsToLocalStorage settings

instance manageProgressAppM :: ManageProgress AppM where
  saveProgress progress = liftEffect $ saveProgressToLocalStorage progress
  completeLevel levelId = do
    store <- getStore
    let
      progress = withCompleteLevel levelId store.progress
    updateStore (ReplaceProgress progress)
    saveProgress progress
