module Validation.Rules.Cut exposing (..)

import Formula exposing (Formula(..))
import Formula.Signed exposing (Signed(..))
import Tableau exposing (..)
import Term exposing (Term(..))
import Validation.Common exposing (..)
import Zipper


areUniform : Signed Formula -> Signed Formula -> Bool
areUniform f1 f2 =
    Formula.Signed.getFormula f1 == Formula.Signed.getFormula f2


checkStructure : Signed Formula -> Signed Formula -> Zipper.Zipper -> Result (List Problem) Zipper.Zipper
checkStructure f1 f2 z =
    not (haveSameSign f1 f2)
        |> resultFromBool z (semanticsProblem z "Both Cut formulas have the same sign")
        |> Result.andThen
            (\_ ->
                areUniform f1 f2
                    |> resultFromBool z (semanticsProblem z "The Cut formulas should differ only in sign")
            )


validate :
    Zipper.Zipper
    -> Zipper.Zipper
    -> Result (List Problem) Zipper.Zipper
validate this other =
    let
        ft =
            this |> checkFormula "Formula"

        fo =
            other |> checkFormula "The other Cut formula"
    in
    this
        |> checkPredicate (hasNumberOfRefs 0)
            (semanticsProblem this "Cut rule must have no references")
        |> Result.map3 checkStructure ft fo
        |> Result.andThen identity
        |> Result.map (always this)
