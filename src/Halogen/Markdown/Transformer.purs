module Halogen.Markdown.Transfomer where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array as Array
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Halogen.HTML as HH
import Halogen.Markdown.AST (Line(..), Lines, Text(..), toHeader)

{-----------------------------------------------------------------------}

normalize :: Lines -> Lines
normalize lines = List.reverse $ tailRec go { accum: lines, result: Nil }
  where
    go { accum: j : k : ls, result: r } =
      case j /\ k of
        TextLine j' /\ TextLine k' ->
          let
            l = TextLine $ j' <> Text "\n" <> k'
          in
           Loop { accum: l : ls, result: r }
        _ ->
          Loop { accum: k : ls, result: j : r }
    go { accum: l : Nil, result: r } = Done (l : r)
    go { accum: Nil, result: r } = Done r

{-----------------------------------------------------------------------}

type TState w a =
  { lines :: Lines
  , elems :: List ( HH.HTML w a )
  }

toHalogen :: forall w a. Lines -> Array ( HH.HTML w a )
toHalogen lines = tailRec go { lines, elems: Nil }
  where
    go :: TState w a -> Step ( TState w a ) ( Array ( HH.HTML w a ) )

    go { lines: l : ls, elems } =
      case l of
        TextLine (Text text) ->
          let
            elem :: HH.HTML w a
            elem = HH.p [ ] [ HH.text text ]
          in
            Loop { lines: ls, elems: elem : elems  }

        Heading level ( Text text ) ->
          let
            elem :: HH.HTML w a
            elem = ( toHeader level ) [ ] [ HH.text text ]
          in
           Loop { lines: ls, elems: elem : elems }

        CodeBlock language (Text text) ->
          let
            elem :: HH.HTML w a
            elem = HH.pre [ ] [ HH.code [ ] [ HH.text text ] ]
          in
            Loop { lines: ls, elems: elem : elems }

        BlankLine ->
          Loop { lines: ls, elems }

    go { lines: Nil, elems } = Done ( Array.fromFoldable elems )

{-----------------------------------------------------------------------}
