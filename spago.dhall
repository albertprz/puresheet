{ name = "puresheet"
, dependencies =
  [ "aff"
  , "argonaut"
  , "argonaut-codecs"
  , "argonaut-generic"
  , "arrays"
  , "bifunctors"
  , "bookhound"
  , "colors"
  , "console"
  , "control"
  , "debug"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "fast-vect"
  , "filterable"
  , "foldable-traversable"
  , "foreign"
  , "halogen"
  , "halogen-hooks"
  , "halogen-hooks-extra"
  , "halogen-router"
  , "halogen-store"
  , "halogen-vdom-string-renderer"
  , "integers"
  , "js-promise-aff"
  , "lists"
  , "matrices"
  , "maybe"
  , "newtype"
  , "numbers"
  , "ordered-collections"
  , "orders"
  , "partial"
  , "point-free"
  , "prelude"
  , "psci-support"
  , "record"
  , "record-extra"
  , "routing-duplex"
  , "safe-coerce"
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
  , "web-storage"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
