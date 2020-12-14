module Validation.Rules.Alpha exposing (..)

import Dict
import Formula exposing (Formula(..))
import Formula.Signed exposing (Signed(..))
import Tableau exposing (..)
import Term exposing (Term(..))
import Validation.Common exposing (..)
import Zipper


validate : Zipper.Zipper -> Result (List Problem) Zipper.Zipper
validate z =
    z
        |> checkPredicate (hasNumberOfRefs 1)
            (semanticsProblem z "α rule must have 1 reference")
        |> Result.andThen (checkReffedFormula "" (Zipper.zFirstRef z))
        |> Result.andThen
            (checkPredicate Formula.Signed.isAlpha
                (semanticsProblem z "Referenced formula is not α")
            )
        |> Result.map2 (\a b -> ( a, b )) (checkFormula "Formula" z)
        |> Result.andThen
            (checkPredicate (\( a, b ) -> Formula.Signed.isSubformulaOf a b)
                (semanticsProblem z
                    ("Is not an α-subformula of ("
                        ++ getReffedId Zipper.zFirstRef z
                        ++ ")."
                    )
                )
            )
        |> Result.map (always z)
