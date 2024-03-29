module Validation.Rules.NCS exposing (..)

import Formula exposing (Formula(..))
import Formula.Signed exposing (Signed(..))
import Tableau exposing (..)
import Term exposing (Term(..))
import Validation.Common exposing (..)
import Zipper exposing (Zipper)


refStructureErr =
    "NCS rule can't be used on referenced formulas"


currentFormulaErr =
    "Formula was not created using the NCS rule"


getNewFormula : Signed Formula -> Signed Formula -> Result String (Signed Formula)
getNewFormula f1 f2 =
    case ( f1, f2 ) of
        ( F (Conj a b), T c ) ->
            if c == a then
                Ok (F b)

            else if c == b then
                Ok (F a)

            else
                Err refStructureErr

        ( T c, F (Conj a b) ) ->
            if c == a then
                Ok (F b)

            else if c == b then
                Ok (F a)

            else
                Err refStructureErr

        _ ->
            Err refStructureErr


check : Signed Formula -> Signed Formula -> Zipper -> Result (List Problem) Zipper
check f1 f2 z =
    checkFormulas currentFormulaErr f1 f2 getNewFormula z


validate : Zipper -> Result (List Problem) Zipper
validate z =
    validate2RefUnary "NCS" check z
