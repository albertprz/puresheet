{ name = "puresheet"
, dependencies =
  [ "aff"
  , "arrays"
  , "bifunctors"
  , "bookhound"
  , "colors"
  , "console"
  , "control"
  , "debug"
  , "distributive"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "fast-vect"
  , "filterable"
  , "foldable-traversable"
  , "foreign"
  , "halogen"
  , "halogen-vdom-string-renderer"
  , "integers"
  , "js-promise-aff"
  , "lists"
  , "matrices"
  , "maybe"
  , "newtype"
  , "numbers"
  , "ordered-collections"
  , "partial"
  , "point-free"
  , "prelude"
  , "psci-support"
  , "record-extra"
  , "strings"
  , "stringutils"
  , "tecton"
  , "tecton-halogen"
  , "transformers"
  , "tree-rose"
  , "tuples"
  , "typelevel-prelude"
  , "unfoldable"
  , "unordered-collections"
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
