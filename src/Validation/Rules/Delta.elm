module Validation.Rules.Delta exposing (..)

import Dict
import Formula exposing (Formula(..))
import Formula.Parser
import Formula.Signed exposing (Signed(..))
import Tableau exposing (..)
import Term exposing (Term(..))
import Validation.Common exposing (..)
import Zipper


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
                            |> getParsedSubst
                        )
                        a
                        b
                )
                (semanticsProblem z
                    ("This is not substituable. Variable '"
                        ++ (z
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
                            |> getParsedSubst
                        )
                        a
                        b
                )
                (semanticsProblem z
                    ("This isn't valid δ-subformula created by substituting '"
                        ++ (z
                                |> getTermsToString
                           )
                        ++ "' for '"
                        ++ (z
                                |> getVarsToString
                           )
                        ++ "' from ("
                        ++ getReffedId Zipper.zFirstRef z
                        ++ ")."
                    )
                )
            )
        |> Result.map (always z)
