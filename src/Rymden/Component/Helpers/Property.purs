module Rymden.Component.Helpers.Property where

import Prelude
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes as SA
import Rymden.Data.Route (Route, route)
import Routing.Duplex (print)
import Web.HTML.Common (ClassName(..))

classes :: forall r i. String -> HP.IProp (class :: String | r) i
classes string = HP.classes $ ClassName <$> split (Pattern " ") string

sclass :: forall r i. String -> HP.IProp (class :: String | r) i
sclass string = SA.classes $ ClassName <$> split (Pattern " ") string

href :: forall r i. Route -> HP.IProp (href :: String | r) i
href = HP.href <<< append "#" <<< print route
