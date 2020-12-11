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
    z |> checkPredicate (hasNumberOfRefs 1)
        (semanticsProblem z "γ must have 1 reference")
    |> Result.andThen (checkReffedFormula "" (Zipper.zFirstRef z))
    |> Result.andThen
        (checkPredicate Formula.Signed.isGamma
            (semanticsProblem z "Referenced formula is not γ")
        )
    |> Result.map (always z)
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
                    ++ getReffedId Zipper.zFirstRef z
                    ++ ")."
                )
            )
        )
    |> Result.map (always z)