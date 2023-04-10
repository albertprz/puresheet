{ name = "halogen-app"
, dependencies =
  [ "arrays"
  , "colors"
  , "dom-indexed"
  , "effect"
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
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
