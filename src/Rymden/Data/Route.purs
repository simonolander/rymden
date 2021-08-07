module Rymden.Data.Route where

import Prelude hiding ((/))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Routing.Duplex (RouteDuplex', root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Route
  = Home
  | Settings

derive instance genericRoute :: Generic Route _

derive instance eqRoute :: Eq Route

instance showRoute :: Show Route where
  show = genericShow

route :: RouteDuplex' Route
route =
  root
    $ sum
        { "Home": noArgs
        , "Settings": "settings" / noArgs
        }
