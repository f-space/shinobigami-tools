{ name = "shinobigami-tools"
, dependencies =
  [ "aff"
  , "arrays"
  , "const"
  , "effect"
  , "enums"
  , "exceptions"
  , "foldable-traversable"
  , "functions"
  , "halogen"
  , "lazy"
  , "maybe"
  , "newtype"
  , "nullable"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "spec"
  , "st"
  , "strings"
  , "transformers"
  , "tuples"
  , "unsafe-coerce"
  , "web-dom"
  , "web-events"
  , "web-html"
  , "web-touchevents"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
