module Validation.Rules.GammaStar exposing (validate)

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
            (semanticsProblem z "γ* rule must have 1 reference")
        |> Result.andThen (checkReffedFormula "" (Zipper.zFirstRef z))
        |> Result.andThen
            (checkPredicate Formula.Signed.isGamma
                (semanticsProblem z "Referenced formula is not γ")
            )
        |> Result.andThen (\_ -> getReffedSignedFormula Zipper.zFirstRef z)
        |> Result.map2 (\newF refF -> ( refF, newF )) (checkFormula "Formula" z)
        |> Result.andThen (\( refF, newF ) -> unaryWithSubstCheck "γ*" refF newF (getParsedSubst z) z)
        |> Result.map (always z)
