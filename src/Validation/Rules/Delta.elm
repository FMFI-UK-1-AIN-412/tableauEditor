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


checkNewVariable : (String -> Bool) -> x -> Zipper.Zipper -> Result x Zipper.Zipper
checkNewVariable pred x z =
    case pred (z |> Zipper.up |> Zipper.zSubstitution |> Maybe.map .term |> Maybe.withDefault "") of
        True ->
            Ok z

        False ->
            Err x


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
        |> Result.andThen (\z1 -> getReffedSignedFormula Zipper.zFirstRef z1)
        |> Result.map (always z)
        |> Result.andThen
            -- checking existing variable above + if new constant is variable or function
            (checkNewVariable
                isNewVariableFunction
                (semanticsProblem z
                    "Your new variable can't be empty or function."
                )
            )
        |> Result.andThen (\z1 -> getReffedSignedFormula Zipper.zFirstRef z1)
        |> Result.map2 (\a b -> ( a, b )) (checkFormula "Formula" z)
        |> Result.andThen
            -- checking substitutable
            (checkPredicate
                (\( a, b ) ->
                    isSubstituable
                        (z
                            |> Zipper.up
                            |> Zipper.zSubstitution
                            |> Maybe.map makeS
                            |> Maybe.withDefault (Dict.fromList [])
                        )
                        a
                        b
                )
                (semanticsProblem z
                    ("This is not substituable. Variable '"
                        ++ (z
                                |> Zipper.up
                                |> Zipper.zSubstitution
                                |> Maybe.map .term
                                |> Maybe.map
                                    (\s ->
                                        if s == "" then
                                            "_"

                                        else
                                            s
                                    )
                                |> Maybe.withDefault ""
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
                            |> Zipper.zSubstitution
                            |> Maybe.map makeS
                            |> Maybe.withDefault (Dict.fromList [])
                        )
                        a
                        b
                )
                (semanticsProblem z
                    ("This isn't valid δ-subformula created by substituting '"
                        ++ (z
                                |> Zipper.up
                                |> Zipper.zSubstitution
                                |> Maybe.map .term
                                |> Maybe.map
                                    (\s ->
                                        if s == "" then
                                            "_"

                                        else
                                            s
                                    )
                                |> Maybe.withDefault ""
                           )
                        ++ "' for '"
                        ++ (z
                                |> Zipper.up
                                |> Zipper.zSubstitution
                                |> Maybe.map .var
                                |> Maybe.map
                                    (\s ->
                                        if s == "" then
                                            "_"

                                        else
                                            s
                                    )
                                |> Maybe.withDefault ""
                           )
                        ++ "' from ("
                        ++ getReffedId Zipper.zFirstRef z
                        ++ ")."
                    )
                )
            )
        |> Result.map (always z)
        |> Result.andThen
            -- checking existing variable above + if new constant is variable or function
            (checkPredicate
                (isNewVariableValid
                    (z
                        |> Zipper.up
                        |> Zipper.zSubstitution
                        |> Maybe.map makeS
                        |> Maybe.withDefault (Dict.fromList [])
                        |> Dict.values
                        |> List.head
                        |> Maybe.withDefault (Var "default")
                        |> Term.toString
                    )
                )
                (semanticsProblem z
                    ("Substituting variable '"
                        ++ (z
                                |> Zipper.up
                                |> Zipper.zSubstitution
                                |> Maybe.map .term
                                |> Maybe.map
                                    (\s ->
                                        if s == "" then
                                            "_"

                                        else
                                            s
                                    )
                                |> Maybe.withDefault ""
                           )
                        ++ "' was located above as free. Please choose another, not used yet. "
                    )
                )
            )
        |> Result.map (always z)
