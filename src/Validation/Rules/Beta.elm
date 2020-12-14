module Validation.Rules.Beta exposing (validateLeft, validateRight)

import Dict
import Errors
import Formula exposing (Formula(..))
import Formula.Signed exposing (Signed(..))
import Tableau exposing (..)
import Term exposing (Term(..))
import Validation.Common exposing (..)
import Zipper


validateLeft : Zipper.Zipper -> Result (List Problem) ( Tableau, Zipper.BreadCrumbs )
validateLeft z =
    validateBeta z (z |> Zipper.up |> Zipper.right)


validateRight : Zipper.Zipper -> Result (List Problem) ( Tableau, Zipper.BreadCrumbs )
validateRight z =
    validateBeta z (z |> Zipper.up |> Zipper.left)


validateBeta :
    Zipper.Zipper
    -> Zipper.Zipper
    -> Result (List Problem) Zipper.Zipper
validateBeta this other =
    let
        ft =
            this |> checkFormula "Formula"

        fo =
            other |> checkFormula "The other β subformula"

        children =
            ft
                |> Result.map List.singleton
                |> Result.map2 (::) fo
                |> Result.map (List.sortBy Formula.Signed.toString)

        -- This is a hack, but defining an ordering on formulas...
        reffed =
            this
                |> checkPredicate (hasNumberOfRefs 1)
                    (semanticsProblem this "Each β must have 1 reference")
                |> Result.andThen (checkReffedFormula "" (Zipper.zFirstRef this))
                |> Result.map (always this)
                |> Result.andThen (\z -> getReffedSignedFormula Zipper.zFirstRef z)
                |> Result.andThen
                    (checkPredicate Formula.Signed.isBeta
                        (semanticsProblem this "Referenced formula is not β")
                    )
                |> Result.map Formula.Signed.subformulas
                |> Result.map (List.sortBy Formula.Signed.toString)
    in
    Errors.merge2 (==) children reffed
        |> Result.andThen (resultFromBool this (semanticsProblem this "Wrong β subformulas."))
        |> Errors.merge2 (always2 this) (betasHaveSameRef this other)


betasHaveSameRef :
    Zipper.Zipper
    -> Zipper.Zipper
    -> Result (List Problem) Zipper.Zipper
betasHaveSameRef this other =
    let
        -- The invalid refs will be reported already
        getRef =
            Zipper.zFirstRef >> .up >> Result.fromMaybe []

        rt =
            getRef this

        ro =
            getRef other
    in
    Errors.merge2 (==) rt ro
        |> Result.andThen
            (resultFromBool this (semanticsProblem this "β references are not the same"))
