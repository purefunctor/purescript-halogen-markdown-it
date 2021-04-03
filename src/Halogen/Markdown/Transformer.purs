module Halogen.Markdown.Transfomer where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)
import DOM.HTML.Indexed (HTMLcode, HTMLp, HTMLpre, Interactive)
import Data.Array as Array
import Data.List (List(..), (:))
import Data.List as List
import Data.Tuple.Nested ((/\))
import Halogen.HTML as HH
import Halogen.Markdown.AST (Level(..), Line(..), Lines, Text(..), toHeader)
import Web.Event.Event (Event)

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

type HTMLh = Interactive ( onScroll :: Event )

type IPropSpec a =
  { h1   :: Array ( HH.IProp HTMLh    a )
  , h2   :: Array ( HH.IProp HTMLh    a )
  , h3   :: Array ( HH.IProp HTMLh    a )
  , h4   :: Array ( HH.IProp HTMLh    a )
  , h5   :: Array ( HH.IProp HTMLh    a )
  , h6   :: Array ( HH.IProp HTMLh    a )
  , p    :: Array ( HH.IProp HTMLp    a )
  , pre  :: Array ( HH.IProp HTMLpre  a )
  , code :: Array ( HH.IProp HTMLcode a )
  }

type TState w a =
  { lines :: Lines
  , elems :: List ( HH.HTML w a )
  }

toHalogen :: forall w a. IPropSpec a -> Lines -> Array ( HH.HTML w a )
toHalogen props lines = ( tailRec go { lines, elems: Nil } )
  where
    go :: TState w a -> Step ( TState w a ) ( Array ( HH.HTML w a ) )

    go { lines: l : ls, elems } =
      case l of
        TextLine (Text text) ->
          let
            elem :: HH.HTML w a
            elem = HH.p ( props.p ) [ HH.text text ]
          in
            Loop { lines: ls, elems: elem : elems  }

        Heading level ( Text text ) ->
          let
            hProps :: Array ( HH.IProp HTMLh a )
            hProps =
              case level of
                H1 -> props.h1
                H2 -> props.h2
                H3 -> props.h3
                H4 -> props.h4
                H5 -> props.h5
                H6 -> props.h6

            elem :: HH.HTML w a
            elem = ( toHeader level ) hProps [ HH.text text ]
          in
            Loop { lines: ls, elems: elem : elems }

        CodeBlock language (Text text) ->
          let
            elem :: HH.HTML w a
            elem = HH.pre props.pre [ HH.code props.code [ HH.text text ] ]
          in
            Loop { lines: ls, elems: elem : elems }

        BlankLine ->
          Loop { lines: ls, elems }

    go { lines: Nil, elems } = Done ( Array.fromFoldable elems )

{-----------------------------------------------------------------------}
