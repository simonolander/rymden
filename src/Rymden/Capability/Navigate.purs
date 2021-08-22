module Rymden.Capability.Navigate where

import Prelude
import Control.Monad.Reader.Trans (lift)
import Halogen (HalogenM)
import Rymden.Data.Route (Route)

class
  Monad m <=
  Navigate m where
  navigate :: Route -> m Unit

instance navigateHalogenM :: Navigate m => Navigate (HalogenM state action slots output m) where
  navigate = lift <<< navigate
