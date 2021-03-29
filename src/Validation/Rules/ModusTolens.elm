module Validation.Rules.ModusTolens exposing (check, validate)

import Formula exposing (Formula(..))
import Formula.Signed exposing (Signed(..))
import Tableau exposing (..)
import Term exposing (Term(..))
import Validation.Common exposing (..)
import Zipper


refStructureErr =
    "MT rule can't be used on referenced formulas"


currentFormulaErr =
    "Formula was not created using the MT rule"


getNewFormula : Signed Formula -> Signed Formula -> Result String (Signed Formula)
getNewFormula f1 f2 =
    case ( f1, f2 ) of
        ( T (Impl a b), F c ) ->
            if b == c then
                Ok (F a)

            else
                Err refStructureErr

        ( F c, T (Impl a b) ) ->
            if b == c then
                Ok (F a)

            else
                Err refStructureErr

        _ ->
            Err refStructureErr


check : Signed Formula -> Signed Formula -> Zipper.Zipper -> Result (List Problem) Zipper.Zipper
check f1 f2 z =
    checkFormulas currentFormulaErr f1 f2 getNewFormula z


validate : Zipper.Zipper -> Result (List Problem) Zipper.Zipper
validate z =
    validate2RefUnary "MT" check z
