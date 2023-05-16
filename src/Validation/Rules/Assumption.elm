module Validation.Rules.Assumption exposing (..)

import Dict
import Formula exposing (Formula(..))
import Formula.Parser
import Formula.Signed exposing (Signed(..), getFormula)
import LogicContext exposing (FormulaCategory(..), contextFormulaCategories)
import Set
import Tableau exposing (..)
import Term exposing (Term(..))
import Validation.Common exposing (..)
import Zipper exposing (Zipper)


isDelta : Zipper -> Bool
isDelta { breadcrumbs } =
    case breadcrumbs of
        (Zipper.UnaryCrumbWithSubst extType _ _) :: _ ->
            case extType of
                Delta ->
                    True

                _ ->
                    False

        _ ->
            False


isUsedInDeltaAbove : String -> Zipper -> Bool
isUsedInDeltaAbove var z =
    let
        parsedF =
            z |> Zipper.zNode |> .value |> Formula.Parser.parseSigned

        wasSubstituted variable zip =
            List.member variable (getParsedSubst zip |> Dict.values |> List.map Term.toString)
    in
    case parsedF of
        Ok _ ->
            isDelta z
                && wasSubstituted var z
                || (z |> Zipper.up)
                /= z
                && isUsedInDeltaAbove var (z |> Zipper.up)

        Err _ ->
            (z |> Zipper.up) /= z && isUsedInDeltaAbove var (z |> Zipper.up)


usedInDeltaErrStr lst =
    "The variable"
        ++ pluralFromList " '" "s '" lst
        ++ String.join "," lst
        ++ pluralFromList "' was" "' were" lst
        ++ " used in δ rule above"


checkFreeVarsUsedInDeltaAbove : Signed Formula -> Zipper -> Result (List Problem) Zipper
checkFreeVarsUsedInDeltaAbove f z =
    let
        freeVars =
            f |> Formula.Signed.getFormula |> Formula.free

        varsUsedInDeltaAbove =
            freeVars |> Set.filter (\var -> isUsedInDeltaAbove var (z |> Zipper.up))
    in
    if Set.isEmpty varsUsedInDeltaAbove then
        Ok z

    else
        Err (semanticsProblem z <| usedInDeltaErrStr <| Set.toList varsUsedInDeltaAbove)


contextAssumptionCheck : Zipper -> Signed Formula -> Result (List Problem) (Signed Formula)
contextAssumptionCheck z sf =
    case z.logicContext of
        Ok ctx ->
            let
                categories =
                    contextFormulaCategories ctx (getFormula sf)
            in
            case sf of
                T _ ->
                    if List.member Axiom categories then
                        Ok sf

                    else if List.member ProovedTheorem categories then
                        Ok sf

                    else
                        Err <| semanticsProblem z "True assumption must be axiom or prooved theorem"

                F _ ->
                    if List.member NewTheorem categories then
                        Ok sf

                    else
                        Err <| semanticsProblem z "False assumption must be theorem that is being prooved"

        Err _ ->
            Ok sf


validate : Zipper -> Result (List Problem) Zipper
validate z =
    z
        |> checkPredicate (hasNumberOfRefs 0)
            (semanticsProblem z "Assumption can't have any references")
        |> Result.andThen (\z1 -> checkFormula "Formula" z1)
        |> Result.andThen (contextAssumptionCheck z)
        |> Result.andThen (\f -> checkFreeVarsUsedInDeltaAbove f z)
        |> Result.map (always z)
