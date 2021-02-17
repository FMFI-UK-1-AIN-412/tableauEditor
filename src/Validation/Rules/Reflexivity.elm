module Validation.Rules.Reflexivity exposing (..)

import Formula exposing (Formula(..))
import Formula.Signed exposing (Signed(..))
import Tableau exposing (..)
import Term exposing (Term(..))
import Validation.Common exposing (..)
import Zipper


isRefl : Zipper.Zipper -> Bool
isRefl z =
    case (Zipper.zNode z).formula of
        Ok (T (EqAtom lt rt)) ->
            if lt == rt then
                True

            else
                False

        _ ->
            False


validate : Zipper.Zipper -> Result (List Problem) Zipper.Zipper
validate z =
    z
        |> checkPredicate (\a -> List.length (Zipper.zNode z).references == 0)
            (semanticsProblem z "Reflexivity rule must have no references")
        |> Result.andThen
            (checkPredicate isRefl
                (semanticsProblem z "Formula is not reflexivity")
            )
        |> Result.map (always z)
