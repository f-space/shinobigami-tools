{ name =
    "shinobigami-tools"
, dependencies =
    [ "halogen", "spec" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
