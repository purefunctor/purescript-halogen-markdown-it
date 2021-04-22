{ name = "halogen-markdown-it"
, dependencies =
  [ "arrays"
  , "bifunctors"
  , "console"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "halogen"
  , "maybe"
  , "lists"
  , "prelude"
  , "psci-support"
  , "simple-markdown-it"
  , "strict-html-parser"
  , "string-parsers"
  , "tailrec"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
