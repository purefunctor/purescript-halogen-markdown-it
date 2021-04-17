let conf = ../spago.dhall

in      conf
    //  { dependencies =
            conf.dependencies # [ "aff", "foreign-object", "halogen-storybook" ]
        , sources = conf.sources # [ "examples/**/*.purs" ]
        }
