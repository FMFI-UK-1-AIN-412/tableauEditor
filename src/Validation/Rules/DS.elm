module Validation.Rules.DS exposing (..)

import Formula exposing (Formula(..))
import Formula.Signed exposing (Signed(..))
import Tableau exposing (..)
import Term exposing (Term(..))
import Validation.Common exposing (..)
import Zipper

--nasl. 4 dat do common
type RuleErrorType = 
    RefFormulasErr
    | CurrentFormulaErr


type alias RuleError = 
    { typ : RuleErrorType
    , msg : String
    }   


ruleErrorToShow: RuleError -> RuleError -> RuleError
ruleErrorToShow e1 e2 = 
    case e1.typ of 
        CurrentFormulaErr ->
            e1

        RefFormulasErr ->
            case e2.typ of
                CurrentFormulaErr ->
                    e2
                _ ->
                    e1


betterOutcome : Result RuleError value -> Result RuleError value -> Result RuleError value
betterOutcome r1 r2 = 
        case r1 of
            Ok val1 ->
               Ok val1
            Err err1 ->
                case r2 of
                    Ok val2 ->
                        Ok val2
                    Err err2 ->
                        Err (ruleErrorToShow err1 err2)
-------------

tryBothOrders: Signed Formula -> Signed Formula -> Zipper.Zipper -> Result (List Problem) Zipper.Zipper
tryBothOrders sf1 sf2 z = 
    let
        currentF = (Zipper.zNode z).formula |> Result.withDefault (T (PredAtom "default" []))
        firstOption = betterOutcome (structureOne sf1 sf2 currentF) (structureOne sf2 sf1 currentF)
        secondOption = betterOutcome (structureTwo sf1 sf2 currentF) (structureTwo sf2 sf1 currentF)
    in
        betterOutcome firstOption secondOption
        |> Result.mapError (\err -> (semanticsProblem z err.msg))
        |> Result.map (always z)


refStructureErr = RuleError RefFormulasErr "DS rule can't be used on referenced formulas"

currentFormulaErr = RuleError CurrentFormulaErr "formula was not created using the DS rule"


structureOne: Signed Formula -> Signed Formula -> Signed Formula -> Result RuleError String
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


structureTwo: Signed Formula -> Signed Formula -> Signed Formula -> Result RuleError String
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


--bude v common
validate : Zipper.Zipper -> Result (List Problem) Zipper.Zipper
validate z =
    z
        |> checkPredicate (hasNumberOfRefs 2)
            (semanticsProblem z "DS rule must have 2 references")
        |> Result.andThen (checkReffedFormula "first" (Zipper.zFirstRef z))
        |> Result.andThen (\_ -> checkReffedFormula "second" (Zipper.zSecondRef z) z)
        |> Result.andThen
            (\_ ->
                tryBothOrders
                    (getReffedSignedFormula Zipper.zFirstRef z
                        |> Result.withDefault (T (PredAtom "default" []))
                    )
                    (getReffedSignedFormula Zipper.zSecondRef z
                        |> Result.withDefault (T (PredAtom "default" []))
                    )
                    z
            )
        |> Result.map (always z) 