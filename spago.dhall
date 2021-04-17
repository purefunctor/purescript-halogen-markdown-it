{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "halogen-markdown"
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
  , "string-parsers"
  , "strings"
  , "tailrec"
  , "tuples"
  , "web-events"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
