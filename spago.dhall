{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "rymden"
, dependencies =
  [ "aff"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "console"
  , "const"
  , "effect"
  , "either"
  , "halogen"
  , "halogen-formless"
  , "halogen-store"
  , "halogen-svg-elems"
  , "maybe"
  , "newtype"
  , "parallel"
  , "prelude"
  , "psci-support"
  , "routing"
  , "routing-duplex"
  , "safe-coerce"
  , "strings"
  , "transformers"
  , "type-equality"
  , "web-html"
  , "web-storage"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
