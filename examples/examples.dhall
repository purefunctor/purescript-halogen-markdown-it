let conf = ../spago.dhall
in conf //
  { dependencies =
      conf.dependencies #
        [ "halogen-storybook"
        ]
  , sources =
      conf.sources #
        [ "examples/**/*.purs"
        ]
  }
