module Validation.Rules.ESFT exposing (validate)

import Formula exposing (Formula(..))
import Formula.Signed exposing (Signed(..))
import Tableau exposing (..)
import Term exposing (Term(..))
import Validation.Common exposing (..)
import Zipper exposing (Zipper)


refStructureErr =
    "ESFT rule can't be used on referenced formulas"


currentFormulaErr =
    "Formula was not created using the ESFT rule"


getNewFormula : Signed Formula -> Signed Formula -> Result String (Signed Formula)
getNewFormula f1 f2 =
    case ( f1, f2 ) of
        ( F (Equiv a b), T c ) ->
            if c == a then
                Ok (F b)

            else if c == b then
                Ok (F a)

            else
                Err refStructureErr

        ( T c, F (Equiv a b) ) ->
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
    validate2RefUnary "ESFT" check z
