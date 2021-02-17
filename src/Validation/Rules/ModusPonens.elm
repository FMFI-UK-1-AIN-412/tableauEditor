module Validation.Rules.ModusPonens exposing (checkFormulaOrder, validate)

import Formula exposing (Formula(..))
import Formula.Signed exposing (Signed(..))
import Tableau exposing (..)
import Term exposing (Term(..))
import Validation.Common exposing (..)
import Zipper


refStructureErr =
    RuleError RefFormulasErr "MP rule can't be used on referenced formulas"


currentFormulaErr =
    RuleError CurrentFormulaErr "formula was not created using the MP rule"


checkFormulaOrder : Signed Formula -> Signed Formula -> Signed Formula -> Result RuleError String
checkFormulaOrder sf1 sf2 currentF =
    case sf1 of
        T (Impl a b) ->
            case sf2 of
                T f2 ->
                    if a == f2 then
                        case currentF of
                            T cf ->
                                if cf == b then
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
    validate2RefUnaryRule "Modus ponens" (tryBothFormulaOrders checkFormulaOrder) z
