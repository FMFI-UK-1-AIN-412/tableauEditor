module Validation.Rules.Reflexivity exposing (..)

import Formula exposing (Formula(..))
import Formula.Signed exposing (Signed(..))
import Term exposing (Term(..))
import Tableau exposing (..)
import Zipper
import Validation.Common exposing(..)

checkIsRefl : Zipper.Zipper -> Bool
checkIsRefl z = 
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
    z |> (checkPredicate (\a -> List.length (Zipper.zNode z).references == 0) 
        (semanticsProblem z "Reflexivity must have no references"))
    |> Result.andThen
        (checkPredicate checkIsRefl
            (semanticsProblem z "Formula is not reflexivity")
        )
    |> Result.map (always z)