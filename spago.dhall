{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-halogen-markdown"
, dependencies =
  [ "console", "effect", "psci-support", "string-parsers" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
