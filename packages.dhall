let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.12-20231123/packages.dhall
        sha256:95ecd1a23305f270971f4d4f2040541559116de6e21aba773d368787f7f1ed35

let overrides =
      { spec-discovery =
        { version = "v8.2.0"
        , repo = "https://github.com/purescript-spec/purescript-spec-discovery"
        , dependencies =
          [ "aff"
          , "aff-promise"
          , "effect"
          , "foldable-traversable"
          , "prelude"
          , "spec"
          ]
        }
      , halogen-router =
        { version = "v0.1.0"
        , repo = "https://github.com/katsujukou/purescript-halogen-router"
        , dependencies =
          [ "aff"
          , "effect"
          , "either"
          , "foreign"
          , "halogen"
          , "halogen-hooks"
          , "halogen-store"
          , "halogen-subscriptions"
          , "maybe"
          , "prelude"
          , "routing"
          , "routing-duplex"
          , "safe-coerce"
          , "transformers"
          , "tuples"
          ]
        }
      }

in  upstream // overrides
