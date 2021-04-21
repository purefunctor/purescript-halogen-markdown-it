let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.1-20210419/packages.dhall sha256:d9a082ffb5c0fabf689574f0680e901ca6f924e01acdbece5eeedd951731375a

let overrides = {=}

let additions =
      { simple-markdown-it =
        { dependencies =
          [ "effect"
          , "foreign"
          , "functions"
          , "options"
          , "psci-support"
          , "untagged-union"
          ]
        , repo =
            "https://github.com/PureFunctor/purescript-simple-markdown-it.git"
        , version = "v0.1.0"
        }
      , strict-html-parser =
        { dependencies =
          [ "arrays"
          , "control"
          , "either"
          , "lists"
          , "prelude"
          , "psci-support"
          , "string-parsers"
          , "strings"
          ]
        , repo =
            "https://github.com/PureFunctor/purescript-strict-html-parser.git"
        , version = "v0.1.0"
        }
      }

in  upstream // overrides // additions
