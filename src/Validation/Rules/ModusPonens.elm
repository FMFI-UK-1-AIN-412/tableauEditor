module Validation.Rules.ModusPonens exposing (check, validate)

import Formula exposing (Formula(..))
import Formula.Signed exposing (Signed(..))
import Tableau exposing (..)
import Term exposing (Term(..))
import Validation.Common exposing (..)
import Zipper exposing (Zipper)


refStructureErr =
    "MP rule can't be used on referenced formulas"


currentFormulaErr =
    "Formula was not created using the MP rule"


getNewFormula : Signed Formula -> Signed Formula -> Result String (Signed Formula)
getNewFormula f1 f2 =
    case ( f1, f2 ) of
        ( T (Impl a b), T c ) ->
            if a == c then
                Ok (T b)

            else
                Err refStructureErr

        ( T c, T (Impl a b) ) ->
            if a == c then
                Ok (T b)

            else
                Err refStructureErr

        _ ->
            Err refStructureErr


check : Signed Formula -> Signed Formula -> Zipper -> Result (List Problem) Zipper
check f1 f2 z =
    checkFormulas currentFormulaErr f1 f2 getNewFormula z


validate : Zipper -> Result (List Problem) Zipper
validate z =
    validate2RefUnary "MP" check z
