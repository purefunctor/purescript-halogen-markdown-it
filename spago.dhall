{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-halogen-markdown"
, dependencies =
  [ "console"
  , "effect"
  , "free"
  , "halogen"
  , "psci-support"
  , "refs"
  , "string-parsers"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
