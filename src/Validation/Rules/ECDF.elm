module Validation.Rules.ECDF exposing (validate)

import Formula exposing (Formula(..))
import Formula.Signed exposing (Signed(..))
import Tableau exposing (..)
import Term exposing (Term(..))
import Validation.Common exposing (..)
import Zipper


getECDFChildren : Signed Formula -> Zipper.Zipper -> Result (List Problem) (List (Signed Formula))
getECDFChildren f z =
    case f of
        F (Equiv a b) ->
            Ok [ T (Conj a (Neg b)), F (Disj a (Neg b)) ]

        _ ->
            Err (semanticsProblem z "Referenced formula is not ECDF")


validate : Zipper.Zipper -> Zipper.Zipper -> Result (List Problem) Zipper.Zipper
validate this other =
    validate2RefBinary "ECDF" getECDFChildren this other
