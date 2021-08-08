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
  , "bifunctors"
  , "console"
  , "const"
  , "debug"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "halogen"
  , "halogen-formless"
  , "halogen-store"
  , "halogen-svg-elems"
  , "integers"
  , "lists"
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
  , "web-events"
  , "web-html"
  , "web-storage"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
