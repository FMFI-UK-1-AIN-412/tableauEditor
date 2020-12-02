module Validation.Rules.Alpha exposing (..)

import Dict
import Formula exposing (Formula(..))
import Formula.Signed exposing (Signed(..))
import Term exposing (Term(..))
import Tableau exposing (..)
import Zipper
import Validation.Common exposing(..)

validate : Zipper.Zipper -> Result (List Problem) Zipper.Zipper
validate z =
    if List.length (Zipper.zNode z).references /= 1 then
            Err (semanticsProblem z "α must have one reference")
    else
        z |> Zipper.getReffed (Zipper.zFirstRef z)
        |> Result.fromMaybe (semanticsProblem z "Invalid reference.")
        |> Result.andThen validateReffedFormula
        |> Result.andThen
            (checkPredicate Formula.Signed.isAlpha
                (semanticsProblem z "Referenced formula is not α")
            )
        |> Result.map2 (\a b -> ( a, b )) (checkFormula "Formula" z)
        |> Result.andThen
            (checkPredicate (\( a, b ) -> Formula.Signed.isSubformulaOf a b)
                (semanticsProblem z
                    ("Is not an α-subformula of ("
                        ++ String.fromInt (Zipper.getReffed (Zipper.zFirstRef z) z |> Maybe.map (Zipper.zNode >> .id) |> Maybe.withDefault 0)
                        ++ ")."
                    )
                )
            )
        |> Result.map (always z)