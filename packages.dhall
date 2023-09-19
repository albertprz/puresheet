let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.10-20230826/packages.dhall
        sha256:9ea8909f5f1219bd716b15f04b4d360cacfb32da9d5ae37a550a01c343b9eb10

let overrides = { spec = upstream.spec // { version = "v7.5.5" } }

in  upstream // overrides
