module Validation.Rules.ModusTolens exposing (checkFormulaOrder, validate)

import Formula exposing (Formula(..))
import Formula.Signed exposing (Signed(..))
import Tableau exposing (..)
import Term exposing (Term(..))
import Validation.Common exposing (..)
import Zipper


refStructureErr =
    RuleError RefFormulasErr "MT rule can't be used on referenced formulas"


currentFormulaErr =
    RuleError CurrentFormulaErr "formula was not created using the MT rule"


checkFormulaOrder : Signed Formula -> Signed Formula -> Signed Formula -> Result RuleError String
checkFormulaOrder sf1 sf2 currentF =
    case sf1 of
        T (Impl a b) ->
            case sf2 of
                F f2 ->
                    if b == f2 then
                        case currentF of
                            F cf ->
                                if cf == a then
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
    validate2RefUnaryRule "Modus tolens" (tryBothFormulaOrders checkFormulaOrder) z
