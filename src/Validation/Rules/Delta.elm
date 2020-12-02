module Validation.Rules.Delta exposing (..)

import Dict
import Set
import Formula exposing (Formula(..))
import Formula.Signed exposing (Signed(..))
import Formula.Parser
import Term exposing (Term(..))
import Tableau exposing (..)
import Zipper
import Validation.Common exposing(..)

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
    if List.length (Zipper.zNode z).references /= 1 then
        Err (semanticsProblem z "Delta must have one reference")
    else
        z |> Zipper.getReffed (Zipper.zFirstRef z)
        |> Result.fromMaybe (semanticsProblem z "Invalid reference.")
        |> Result.andThen validateReffedFormula
        |> Result.andThen
            (checkPredicate Formula.Signed.isDelta
                (semanticsProblem z "Referenced formula is not delta")
            )
        |> Result.map (always z)
        |> Result.andThen
            (checkIsPointingOnSelf
                (isPointingOnSelf Zipper.zFirstRef)
                (semanticsProblem z "delta can not be pointing on itself")
            )
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
                        ++ String.fromInt (Zipper.getReffed (Zipper.zFirstRef z) z |> Maybe.map (Zipper.zNode >> .id) |> Maybe.withDefault 0)
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
                    ("This isn't valid delta-subformula created by substituting '"
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
                        ++ String.fromInt (Zipper.getReffed (Zipper.zFirstRef z) z |> Maybe.map (Zipper.zNode >> .id) |> Maybe.withDefault 0)
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