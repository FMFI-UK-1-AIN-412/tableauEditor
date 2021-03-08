module Validation.Rules.Delta exposing (..)

import Dict
import Formula exposing (Formula(..))
import Formula.Parser
import Formula.Signed exposing (Signed(..))
import Set
import Tableau exposing (..)
import Term exposing (Term(..))
import Validation.Common exposing (..)
import Zipper


notVarsErrStr lst =
    "The function"
        ++ (if List.length lst > 1 then
                "s '"

            else
                " '"
           )
        ++ String.join "," (List.map Term.toString lst)
        ++ (if List.length lst > 1 then
                "' are"

            else
                "' is"
           )
        ++ " supposed to be "
        ++ (if List.length lst > 1 then
                "variables"

            else
                "a variable"
           )


similarAboveErrStr lst =
    "The variable"
        ++ (if List.length lst > 1 then
                "s "

            else
                " "
           )
        ++ "'"
        ++ String.join "," lst
        ++ "'"
        ++ (if List.length lst > 1 then
                " were"

            else
                " was"
           )
        ++ " located above as free"


isFunction : Term -> Bool
isFunction term =
    case term of
        Var _ ->
            False

        Fun _ _ ->
            True


checkNewVariables : Zipper.Zipper -> Result (List Problem) Zipper.Zipper
checkNewVariables z =
    case Zipper.up z |> Zipper.zSubstitution of
        Just subst ->
            case subst.parsedSubst of
                Ok parsed ->
                    let
                        terms =
                            Dict.values parsed

                        termsToString =
                            List.map Term.toString terms
                    in
                    case List.filter isFunction terms of
                        [] ->
                            case List.filter (\var -> isSimilarAbove var (z |> Zipper.up)) termsToString of
                                [] ->
                                    Ok z

                                vars ->
                                    Err (semanticsProblem z (similarAboveErrStr vars))

                        functions ->
                            Err (semanticsProblem z (notVarsErrStr functions))

                Err _ ->
                    Err (semanticsProblem z "substitution parsing failed")

        Nothing ->
            Err (semanticsProblem z "node contains no substitution")


isSimilarAbove : String -> Zipper.Zipper -> Bool
isSimilarAbove var z =
    let
        parsedF =
            z |> Zipper.zNode |> .value |> Formula.Parser.parseSigned
    in
    case parsedF of
        Ok parsed ->
            Set.member var (Formula.free (parsed |> Formula.Signed.getFormula))
                || (z |> Zipper.up)
                /= z
                && isSimilarAbove var (z |> Zipper.up)

        Err _ ->
            (z |> Zipper.up) /= z && isSimilarAbove var (z |> Zipper.up)


validate :
    Zipper.Zipper
    -> Result (List Problem) ( Tableau, Zipper.BreadCrumbs )
validate z =
    z
        |> checkPredicate (hasNumberOfRefs 1)
            (semanticsProblem z "δ rule must have 1 reference")
        |> Result.andThen (checkReffedFormula "" (Zipper.zFirstRef z))
        |> Result.andThen
            (checkPredicate Formula.Signed.isDelta
                (semanticsProblem z "Referenced formula is not δ")
            )
        |> Result.map (always z)
        |> Result.andThen
            (checkPredicate (\z1 -> numberOfSubstPairs z1 <= 1)
                (semanticsProblem z "δ rule must be used with at most 1 substitution pair")
            )
        |> Result.andThen
            checkNewVariables
        |> Result.andThen (\z1 -> getReffedSignedFormula Zipper.zFirstRef z1)
        |> Result.map2 (\a b -> ( a, b )) (checkFormula "Formula" z)
        |> Result.andThen
            -- checking substitutable
            (checkPredicate
                (\( a, b ) ->
                    isSubstituable
                        (z
                            |> Zipper.up
                            |> getParsedSubst
                        )
                        a
                        b
                )
                (semanticsProblem z
                    ("This is not substituable. Variable '"
                        ++ (z
                                |> Zipper.up
                                |> getTermsToString
                           )
                        ++ "' is bound in referrenced formula ("
                        ++ getReffedId Zipper.zFirstRef z
                        ++ "). Choose another variable."
                    )
                )
            )
        |> Result.andThen
            -- checking valid substitution
            (checkPredicate
                (\( a, b ) ->
                    substitutionIsValid
                        (z
                            |> Zipper.up
                            |> getParsedSubst
                        )
                        a
                        b
                )
                (semanticsProblem z
                    ("This isn't valid δ-subformula created by substituting '"
                        ++ (z
                                |> Zipper.up
                                |> getTermsToString
                           )
                        ++ "' for '"
                        ++ (z
                                |> Zipper.up
                                |> getVarsToString
                           )
                        ++ "' from ("
                        ++ getReffedId Zipper.zFirstRef z
                        ++ ")."
                    )
                )
            )
        |> Result.map (always z)
