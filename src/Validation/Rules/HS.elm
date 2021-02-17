module Validation.Rules.HS exposing (..)

import Formula exposing (Formula(..))
import Formula.Signed exposing (Signed(..))
import Tableau exposing (..)
import Term exposing (Term(..))
import Validation.Common exposing (..)
import Zipper


refStructureErr =
    RuleError RefFormulasErr "HS rule can't be used on referenced formulas"


currentFormulaErr =
    RuleError CurrentFormulaErr "formula was not created using the HS rule"


checkFormulaOrder : Signed Formula -> Signed Formula -> Signed Formula -> Result RuleError String
checkFormulaOrder sf1 sf2 currentF =
    case sf1 of
        T (Impl a b) ->
            case sf2 of
                T (Impl c d) ->
                    if b == c then
                        case currentF of
                            T (Impl x y) ->
                                if x == a && y == d then
                                    Ok "ok"

                                else
                                    Err currentFormulaErr

                            _ ->
                                Err currentFormulaErr

                    else
                        Err refStructureErr

                _ ->
                    Err refStructureErr

        _ ->
            Err refStructureErr


validate : Zipper.Zipper -> Result (List Problem) Zipper.Zipper
validate z =
    validate2RefUnaryRule "HS" (tryBothFormulaOrders checkFormulaOrder) z