module Validation.Rules.ECDF exposing (validate)

import Formula exposing (Formula(..))
import Formula.Signed exposing (Signed(..))
import Tableau exposing (..)
import Term exposing (Term(..))
import Validation.Common exposing (..)
import Zipper exposing (Zipper)


getECDFChildren : Signed Formula -> Zipper -> Result (List Problem) (List (Signed Formula))
getECDFChildren f z =
    case f of
        F (Equiv a b) ->
            Ok [ T (Conj a (Neg b)), F (Disj a (Neg b)) ]

        _ ->
            Err (semanticsProblem z "Referenced formula is not ECDF")


validate : Zipper -> Zipper -> Result (List Problem) Zipper
validate this other =
    validate2RefBinary "ECDF" getECDFChildren this other
