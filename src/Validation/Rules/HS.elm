module Validation.Rules.HS exposing (..)

import Formula exposing (Formula(..))
import Formula.Signed exposing (Signed(..))
import Tableau exposing (..)
import Term exposing (Term(..))
import Validation.Common exposing (..)
import Zipper exposing (Zipper)


refStructureErr =
    "HS rule can't be used on referenced formulas"


currentFormulaErr =
    "Formula was not created using the HS rule"


getNewFormula : Signed Formula -> Signed Formula -> Result String (Signed Formula)
getNewFormula f1 f2 =
    case ( f1, f2 ) of
        ( T (Impl a b), T (Impl c d) ) ->
            if b == c then
                Ok (T (Impl a d))

            else if d == a then
                Ok (T (Impl c b))

            else
                Err refStructureErr

        _ ->
            Err refStructureErr


check : Signed Formula -> Signed Formula -> Zipper -> Result (List Problem) Zipper
check f1 f2 z =
    checkFormulas currentFormulaErr f1 f2 getNewFormula z


validate : Zipper -> Result (List Problem) Zipper
validate z =
    validate2RefUnary "HS" check z
