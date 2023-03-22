module Validation.Rules.ECDT exposing (validate)

import Formula exposing (Formula(..))
import Formula.Signed exposing (Signed(..))
import Tableau exposing (..)
import Term exposing (Term(..))
import Validation.Common exposing (..)
import Zipper exposing (Zipper)


getECDTchildren : Signed Formula -> Zipper -> Result (List Problem) (List (Signed Formula))
getECDTchildren f z =
    case f of
        T (Equiv a b) ->
            Ok [ T (Conj a b), F (Disj a b) ]

        _ ->
            Err (semanticsProblem z "Referenced formula is not ECDT")


validate : Zipper -> Zipper -> Result (List Problem) Zipper
validate this other =
    validate2RefBinary "ECDT" getECDTchildren this other
