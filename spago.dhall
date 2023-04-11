{ name = "halogen-app"
, dependencies =
  [ "aff"
  , "arrays"
  , "bifunctors"
  , "colors"
  , "dom-indexed"
  , "effect"
  , "foldable-traversable"
  , "halogen"
  , "integers"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "strings"
  , "tecton"
  , "tecton-halogen"
  , "tuples"
  , "typelevel-prelude"
  , "web-dom"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
