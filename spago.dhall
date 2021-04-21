{ name = "halogen-markdown-it"
, dependencies =
  [ "arrays"
  , "console"
  , "control"
  , "dom-indexed"
  , "effect"
  , "either"
  , "halogen"
  , "lists"
  , "maybe"
  , "newtype"
  , "prelude"
  , "psci-support"
  , "simple-markdown-it"
  , "strict-html-parser"
  , "string-parsers"
  , "strings"
  , "tailrec"
  , "tuples"
  , "web-events"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
