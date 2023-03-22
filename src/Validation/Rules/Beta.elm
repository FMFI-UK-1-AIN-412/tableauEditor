module Validation.Rules.Beta exposing (validate)

import Dict
import Errors
import Formula exposing (Formula(..))
import Formula.Signed exposing (Signed(..))
import Tableau exposing (..)
import Term exposing (Term(..))
import Validation.Common exposing (..)
import Zipper exposing (Zipper)


getBetaChildren f z =
    f
        |> checkPredicate Formula.Signed.isBeta
            (semanticsProblem z "Referenced formula is not β")
        |> Result.map Formula.Signed.subformulas


validate :
    Zipper
    -> Zipper
    -> Result (List Problem) Zipper
validate this other =
    validate2RefBinary "Beta" getBetaChildren this other
