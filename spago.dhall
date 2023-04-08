{ name = "halogen-project"
, dependencies =
  [ "prelude"
  , "effect"
  , "halogen"
  , "tecton-halogen"
  , "tecton"
  , "colors"
  , "maybe"
  , "tuples"
  , "typelevel-prelude"
  , "arrays"
  , "ordered-collections"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
