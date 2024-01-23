module Validation.Rules.Gamma exposing (..)

import Dict
import Formula exposing (Formula(..))
import Formula.Parser
import Formula.Signed exposing (Signed(..))
import Set
import Tableau exposing (..)
import Term exposing (Term(..))
import Validation.Common exposing (..)
import Zipper exposing (Zipper)


validate :
    Zipper
    -> Result (List Problem) Zipper
validate z =
    z
        |> checkPredicate (hasNumberOfRefs 1)
            (semanticsProblem z "γ rule must have 1 reference")
        |> Result.andThen (checkReffedFormula "" (Zipper.zFirstRef z))
        |> Result.andThen
            (checkPredicate Formula.Signed.isGamma
                (semanticsProblem z "Referenced formula is not γ")
            )
        |> Result.map (always z)
        |> Result.andThen
            (checkPredicate (\z1 -> numberOfSubstPairs z1 <= 1)
                (semanticsProblem z "γ rule must be used with at most 1 substitution pair")
            )
        |> Result.andThen (\z1 -> getReffedSignedFormula Zipper.zFirstRef z1)
        |> Result.map2 (\a b -> ( a, b )) (checkFormula "Formula" z)
        |> Result.andThen
            -- checking substituable
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
                    ("This isn't valid γ-subformula created by substituting '"
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
