module Validation.Rules.DS exposing (..)

import Formula exposing (Formula(..))
import Formula.Signed exposing (Signed(..))
import Tableau exposing (..)
import Term exposing (Term(..))
import Validation.Common exposing (..)
import Zipper


refStructureErr =
    RuleError RefFormulasErr "DS rule can't be used on referenced formulas"


currentFormulaErr =
    RuleError CurrentFormulaErr "formula was not created using the DS rule"


structureOne : Signed Formula -> Signed Formula -> Signed Formula -> Result RuleError String
structureOne sf1 sf2 currentF =
    case sf1 of
        T (Disj a b) ->
            case sf2 of
                F f2 ->
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


structureTwo : Signed Formula -> Signed Formula -> Signed Formula -> Result RuleError String
structureTwo sf1 sf2 currentF =
    case sf1 of
        T (Disj a b) ->
            case sf2 of
                F f2 ->
                    if b == f2 then
                        case currentF of
                            T cf ->
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
    validate2RefUnaryRule "DS" (tryBothOrdersAndStructures structureOne structureTwo) z
