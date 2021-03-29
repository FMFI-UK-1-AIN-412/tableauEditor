module Validation.Rules.ESTT exposing (validate)

import Formula exposing (Formula(..))
import Formula.Signed exposing (Signed(..))
import Tableau exposing (..)
import Term exposing (Term(..))
import Validation.Common exposing (..)
import Zipper


refStructureErr =
    "ESTT rule can't be used on referenced formulas"


currentFormulaErr =
    "Formula was not created using the ESTT rule"


getNewFormula : Signed Formula -> Signed Formula -> Result String (Signed Formula)
getNewFormula f1 f2 =
    case ( f1, f2 ) of
        ( T (Equiv a b), T c ) ->
            if c == a then
                Ok (T b)

            else if c == b then
                Ok (T a)

            else
                Err refStructureErr

        ( T c, T (Equiv a b) ) ->
            if c == a then
                Ok (T b)

            else if c == b then
                Ok (T a)

            else
                Err refStructureErr

        _ ->
            Err refStructureErr


check : Signed Formula -> Signed Formula -> Zipper.Zipper -> Result (List Problem) Zipper.Zipper
check f1 f2 z =
    checkFormulas currentFormulaErr f1 f2 getNewFormula z


validate : Zipper.Zipper -> Result (List Problem) Zipper.Zipper
validate z =
    validate2RefUnary "ESTT" check z
