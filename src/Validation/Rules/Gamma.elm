module Validation.Rules.Gamma exposing (..)

import Dict
import Set
import Formula exposing (Formula(..))
import Formula.Signed exposing (Signed(..))
import Formula.Parser
import Term exposing (Term(..))
import Tableau exposing (..)
import Zipper
import Validation.Common exposing(..)


validate :
    Zipper.Zipper
    -> Result (List Problem) ( Tableau, Zipper.BreadCrumbs )
validate z =
    if List.length (Zipper.zNode z).references /= 1 then
        Err (semanticsProblem z "γ must have one reference")
    else
         z |> Zipper.getReffed (Zipper.zFirstRef z)
        |> Result.fromMaybe (semanticsProblem z "Invalid reference.")
        |> Result.andThen validateReffedFormula
        |> Result.andThen
            (checkPredicate Formula.Signed.isGamma
                (semanticsProblem z "Referenced formula is not γ")
            )
        |> Result.map (always z)
        |> Result.andThen
            (checkIsPointingOnSelf
                (isPointingOnSelf Zipper.zFirstRef)
                (semanticsProblem z "γ can not be pointing on itself")
            )
        |> Result.andThen (\z1 -> getReffedSignedFormula Zipper.zFirstRef z1)
        |> Result.map2 (\a b -> ( a, b )) (checkFormula "Formula" z)
        |> Result.andThen
            -- checking substituable
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
                    ("This isn't valid γ-subformula created by substituting '"
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