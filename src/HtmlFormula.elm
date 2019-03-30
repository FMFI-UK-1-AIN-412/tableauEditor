module HtmlFormula exposing (..)

import Formula exposing (..)
import Html exposing (..)

joinMapA : b -> (a -> List b -> List b) -> List a -> List b -> List b
joinMapA glue fa xs acc =
  case xs of
    [] -> acc
    [y] -> fa y acc
    y :: ys -> fa y (List.foldr (\y acc -> glue :: fa y acc) acc ys)

htmlSigned sf =
  case sf of
    T f -> strong [] [text "T"] :: text " " :: htmlFormula f []
    F f -> strong [] [text "F"] :: text " " :: htmlFormula f []

htmlFormula f hs =
  let
    htmlBinF lf c rf =
      htmlParen
       <| htmlFormula lf
           << (::) (text <| " " ++ c ++ " ")
           << htmlFormula rf

    --htmlQF q bv f =
    --  (::) (text q) <| htmlVar bv <| atomSpace f <| htmlFormula f hs
    --
    --atomSpace f hs =
    --  case f of
    --    Atom _ -> text " " :: hs
    --    --Atom _ _ -> text " " :: hs
    --    _ -> hs
  in
    case f of
      FT -> text "True" :: hs
      FF -> text "False" :: hs
      Atom a -> htmlVar a hs
      --Atom p [] -> htmlVar p hs
      --Atom p ts -> text p :: htmlArgs ts hs
      Neg f -> text "¬" :: htmlFormula f hs
      Conj lf rf -> htmlBinF lf "∧" rf hs
      Disj lf rf -> htmlBinF lf "∨" rf hs
      Impl lf rf -> htmlBinF lf "→" rf hs
      --ForAll bv f -> htmlQF "∀" bv f
      --Exists bv f -> htmlQF "∃" bv f

--htmlArgs ts =
--  htmlParen (htmlTerms ts)

htmlParen : (List (Html msg) -> List (Html msg)) -> List (Html msg) -> List (Html msg)
htmlParen content hs =
  text "(" :: content (text ")" :: hs)

--htmlTerms : List Term -> List (Html msg) -> List (Html msg)
--htmlTerms ts = joinMapA (text ", ") htmlTerm ts
--
--htmlTerm t hs =
--  case t of
--    Var v -> htmlVar v hs
--    Fun f ts -> text f :: htmlArgs ts hs

htmlVar name hs =
  var [] [ text name ] :: hs

{- vim: set sw=2 ts=2 sts=2 et : -}
