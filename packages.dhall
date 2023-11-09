let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.12-20231109/packages.dhall
        sha256:f91a6a26ed0ad8402db57aa9e7e21d20224bf56018e7ba1728bcdbee584a9628

let overrides =
      { spec-discovery.version = "v8.2.0"
      , spec-discovery.dependencies
        =
        [ "aff"
        , "aff-promise"
        , "effect"
        , "foldable-traversable"
        , "prelude"
        , "spec"
        ]
      , spec-discovery.repo
        = "https://github.com/purescript-spec/purescript-spec-discovery"
      }

in  upstream // overrides
