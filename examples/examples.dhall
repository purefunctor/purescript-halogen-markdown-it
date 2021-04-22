let conf = ../spago.dhall

in      conf
    //  { dependencies =
              conf.dependencies
            # [ "aff", "foreign-object", "halogen-storybook", "maybe" ]
        , sources = conf.sources # [ "examples/**/*.purs" ]
        }
