module Validation.Rules.ESFT exposing (validate)

import Formula exposing (Formula(..))
import Formula.Signed exposing (Signed(..))
import Tableau exposing (..)
import Term exposing (Term(..))
import Validation.Common exposing (..)
import Zipper


refStructureErr = RuleError RefFormulasErr "ESFT rule can't be used on referenced formulas"

currentFormulaErr = RuleError CurrentFormulaErr "formula was not created using the ESFT rule"


structureOne: Signed Formula -> Signed Formula -> Signed Formula -> Result RuleError String
structureOne sf1 sf2 currentF = 
        case sf1 of
            F (Equiv a b) -> 
                case sf2 of
                    T f2 ->
                        if a == f2 then
                            case currentF of
                                F cf ->
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


structureTwo: Signed Formula -> Signed Formula -> Signed Formula -> Result RuleError String
structureTwo sf1 sf2 currentF = 
        case sf1 of
            F (Equiv a b) -> 
                case sf2 of
                    T f2 ->
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
    validate2RefUnaryRule "ESFT" (tryBothOrdersAndStructures structureOne structureTwo) z