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
  , "debug"
  , "effect"
  , "either"
  , "halogen"
  , "halogen-formless"
  , "halogen-store"
  , "halogen-svg-elems"
  , "integers"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "parallel"
  , "prelude"
  , "psci-support"
  , "random"
  , "routing"
  , "routing-duplex"
  , "safe-coerce"
  , "strings"
  , "transformers"
  , "tuples"
  , "type-equality"
  , "web-html"
  , "web-storage"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
