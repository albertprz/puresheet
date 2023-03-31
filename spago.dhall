{ name = "halogen-project"
, dependencies =
  [ "prelude", "effect", "halogen", "tecton-halogen", "tecton", "colors" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
