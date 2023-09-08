{ name = "puresheet"
, dependencies =
  [ "aff"
  , "arrays"
  , "bifunctors"
  , "bookhound"
  , "colors"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "filterable"
  , "foldable-traversable"
  , "halogen"
  , "integers"
  , "js-promise-aff"
  , "matrices"
  , "maybe"
  , "ordered-collections"
  , "partial"
  , "point-free"
  , "prelude"
  , "psci-support"
  , "strings"
  , "tecton"
  , "tecton-halogen"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "unsafe-coerce"
  , "web-clipboard"
  , "web-dom"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
