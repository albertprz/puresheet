let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.12-20231120/packages.dhall
        sha256:4c066f08ee174c39f5a65f7899f8e3dafdf75c416747eda8a6c6f47e4ac4faaa

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
