module Validation.Rules.DeltaStar exposing (validate)

import Formula exposing (Formula(..))
import Formula.Signed exposing (Signed(..))
import Tableau exposing (..)
import Term exposing (Term(..))
import Validation.Common exposing (..)
import Zipper


validate :
    Zipper.Zipper
    -> Result (List Problem) Zipper.Zipper
validate z =
    z
        |> checkPredicate (hasNumberOfRefs 1)
            (semanticsProblem z "δ* rule must have 1 reference")
        |> Result.andThen (checkReffedFormula "" (Zipper.zFirstRef z))
        |> Result.andThen
            (checkPredicate Formula.Signed.isDelta
                (semanticsProblem z "Referenced formula is not δ")
            )
        |> Result.andThen (\_ -> checkNewVariables z)
        |> Result.andThen (\z1 -> getReffedSignedFormula Zipper.zFirstRef z1)
        |> Result.map2 (\newF refF -> ( refF, newF )) (checkFormula "Formula" z)
        |> Result.andThen (\( refF, newF ) -> unaryWithSubstCheck "δ*" refF newF (getParsedSubst z) z)
        |> Result.map (always z)
