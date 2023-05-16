module Validation.Rules.Leibnitz exposing (checkSubsts, commonSignedTemplate, validate)

import Dict
import Formula exposing (Formula(..))
import Formula.Signed exposing (Signed(..))
import Tableau exposing (..)
import Term exposing (Term(..))
import Validation.Common exposing (..)
import Zipper exposing (Zipper)
import Helpers.Rules exposing (signedMap)
import Html exposing (a)


templateVar : Term
templateVar =
    Var "[]"


differentFormulaStructure : Formula -> Formula -> String
differentFormulaStructure refF currentF =
    "The 2nd referenced formula and current formula have different structure ("
    ++ Formula.toString refF ++ " vs. "
    ++ Formula.toString currentF ++ ")"


differentTermStructure : Term -> Term -> String
differentTermStructure refTerm currentTerm =
    "The 2nd referenced formula and current formula contain non-matching subterms at corresponding locations ("
    ++ Term.toString refTerm ++ " vs. "
    ++ Term.toString currentTerm ++ ")"


differentArgsCount : String
differentArgsCount =
    "Arguments count mismatch"


differentArgsCountTo : String -> String -> String
differentArgsCountTo replacementMsg msg =
    if msg == differentArgsCount then
        replacementMsg

    else
        msg


differentSign : String
differentSign =
    "The 2nd referenced formula and current formula have different sign"


isEquality : Signed Formula -> Bool
isEquality f =
    case f of
        T (EqAtom _ _) ->
            True

        _ ->
            False


leftEqTerm : Signed Formula -> Term
leftEqTerm f =
    case f of
        T (EqAtom lt _) ->
            lt

        _ ->
            Fun "default" []


rightEqTerm : Signed Formula -> Term
rightEqTerm f =
    case f of
        T (EqAtom _ rt) ->
            rt

        _ ->
            Fun "default" []


commonTermsTemplate : Term -> Term -> List Term -> List Term -> Result String (List Term)
commonTermsTemplate lTerm rTerm refTerms currentTerms =
    case (refTerms, currentTerms) of
        (refTerm :: rts, currentTerm :: cts) ->
            Result.map2 (::)
                (commonTermTemplate lTerm rTerm refTerm currentTerm)
                (commonTermsTemplate lTerm rTerm rts cts)

        ([], []) ->
            Ok <| []

        _ ->
            Err differentArgsCount


commonTermTemplate : Term -> Term -> Term -> Term -> Result String Term
commonTermTemplate lTerm rTerm refTerm currentTerm =
    if refTerm == lTerm && currentTerm == rTerm then
        Ok templateVar
    else
        case (refTerm, currentTerm) of
            (Var refVSym, Var currentVSym) ->
                if refVSym == currentVSym then
                    Ok <| Var refVSym

                else
                    Err <| differentTermStructure refTerm currentTerm

            (Fun refFSym refTerms, Fun currentFSym currentTerms) ->
                if refFSym == currentFSym
                && List.length refTerms == List.length currentTerms then
                    commonTermsTemplate lTerm rTerm refTerms currentTerms
                        |> Result.map (Fun refFSym)
                        |> Result.mapError
                            (differentArgsCountTo <|
                                differentTermStructure refTerm currentTerm)

                else
                    Err <| differentTermStructure refTerm currentTerm

            _ ->
                Err <| differentTermStructure refTerm currentTerm


commonFormulaTemplate : Term -> Term -> Formula -> Formula -> Result String Formula
commonFormulaTemplate lTerm rTerm refF currentF =
    case (refF, currentF) of
        (PredAtom refPSym refTerms, PredAtom currentPSym currentTerms) ->
            if refPSym == currentPSym
            && List.length refTerms == List.length currentTerms then
                commonTermsTemplate lTerm rTerm refTerms currentTerms
                    |> Result.map (PredAtom refPSym)
                    |> Result.mapError
                        (differentArgsCountTo <|
                            differentFormulaStructure refF currentF)

            else
                Err <| differentFormulaStructure refF currentF

        (EqAtom refLt refRt, EqAtom currentLt currentRt) ->
            Result.map2 EqAtom
                (commonTermTemplate lTerm rTerm refLt currentLt)
                (commonTermTemplate lTerm rTerm refRt currentRt)

        (Neg refSf, Neg currentSf) ->
            Result.map Neg
                (commonFormulaTemplate lTerm rTerm refSf currentSf)

        (Conj refSf1 refSf2, Conj currentSf1 currentSf2) ->
            Result.map2 Conj
                (commonFormulaTemplate lTerm rTerm refSf1 currentSf1)
                (commonFormulaTemplate lTerm rTerm refSf2 currentSf2)

        (Disj refSf1 refSf2, Disj currentSf1 currentSf2) ->
            Result.map2 Disj
                (commonFormulaTemplate lTerm rTerm refSf1 currentSf1)
                (commonFormulaTemplate lTerm rTerm refSf2 currentSf2)

        (Impl refSf1 refSf2, Impl currentSf1 currentSf2) ->
            Result.map2 Impl
                (commonFormulaTemplate lTerm rTerm refSf1 currentSf1)
                (commonFormulaTemplate lTerm rTerm refSf2 currentSf2)

        (Equiv refSf1 refSf2, Equiv currentSf1 currentSf2) ->
            Result.map2 Equiv
                (commonFormulaTemplate lTerm rTerm refSf1 currentSf1)
                (commonFormulaTemplate lTerm rTerm refSf2 currentSf2)

        (ForAll refX refSf, ForAll currentX currentSf) ->
            if refX == currentX then
                Result.map (ForAll refX)
                    (commonFormulaTemplate lTerm rTerm refSf currentSf)

            else
                Err <| differentFormulaStructure refF currentF

        (Exists refX refSf, Exists currentX currentSf) ->
            if refX == currentX then
                Result.map (Exists refX)
                    (commonFormulaTemplate lTerm rTerm refSf currentSf)

            else
                Err <| differentFormulaStructure refF currentF

        _ ->
            Err <| differentFormulaStructure refF currentF


commonSignedTemplate : Term -> Term -> Signed Formula -> Signed Formula -> Result String (Signed Formula)
commonSignedTemplate lTerm rTerm refSF currentSF =
    case (refSF, currentSF) of
        (T refF, T currentF) ->
            Result.map T
                <| commonFormulaTemplate lTerm rTerm refF currentF

        (F refF, F currentF) ->
            Result.map F
                <| commonFormulaTemplate lTerm rTerm refF currentF

        _ ->
            Err <| differentSign


applyFunToSigned : (a -> Result e a) -> Signed a -> Result e (Signed a)
applyFunToSigned function sf =
    case sf of
        T formula ->
            Result.map T <| function formula

        F formula ->
            Result.map F <| function formula


mapNotSubstitutableError : Term -> Term -> Term -> Zipper -> String -> List Problem
mapNotSubstitutableError substitutedT lhsT rhsT z err =
    semanticsProblem z
        <| String.replace
            (Term.toString substitutedT ++ " for []")
            (Term.toString lhsT ++ " with "
            ++ Term.toString rhsT
            ++ " in the 2nd referenced formula")
            err


checkSubst :
    Term.Substitution
    -> (String -> List Problem)
    -> Result (List Problem) (Signed Formula)
    -> Signed Formula
    -> Zipper
    -> Result (List Problem) Zipper
checkSubst σ mapErr replaced currentF z =
    case replaced of
        Err problem ->
            Err problem

        Ok repl ->
            applyFunToSigned (Formula.substitute σ) repl
                |> Result.mapError mapErr
                |> Result.andThen
                    (checkPredicate (\f -> f == currentF)
                        (semanticsProblem z "Substitution invalid")
                    )
                |> Result.map (always z)


checkSubsts :
    Signed Formula
    -> Signed Formula
    -> Zipper
    -> Result (List Problem) Zipper
checkSubsts refEq refF z =
    let
        lt =
            leftEqTerm refEq

        rt =
            rightEqTerm refEq

        currentF =
            (Zipper.zNode z).formula |> Result.withDefault (T (PredAtom "default" []))

        replaced =
            commonSignedTemplate lt rt refF currentF |> Result.mapError (\err -> semanticsProblem z err)

        σ1 =
            Dict.fromList [ ( "[]", lt ) ]

        mapErr1 =
            mapNotSubstitutableError lt lt rt z

        σ2 =
            Dict.fromList [ ( "[]", rt ) ]

        mapErr2 =
            mapNotSubstitutableError rt lt rt z

    in
    z
        |> checkSubst σ1 mapErr1 replaced refF
        |> Result.andThen (checkSubst σ2 mapErr2 replaced currentF)


validate : Zipper -> Result (List Problem) Zipper
validate z =
    z
        |> checkPredicate (hasNumberOfRefs 2)
            (semanticsProblem z "Leibnitz rule must have 2 references")
        |> Result.andThen (checkReffedFormula "first" (Zipper.zFirstRef z))
        |> Result.andThen
            (checkPredicate isEquality
                (semanticsProblem z "first referenced formula is not equality")
            )
        |> Result.andThen (\_ -> checkReffedFormula "second" (Zipper.zSecondRef z) z)
        |> Result.andThen
            (\_ ->
                checkSubsts
                    (getReffedSignedFormula Zipper.zFirstRef z
                        |> Result.withDefault (T (PredAtom "default" []))
                    )
                    (getReffedSignedFormula Zipper.zSecondRef z
                        |> Result.withDefault (T (PredAtom "default" []))
                    )
                    z
            )
        |> Result.map (always z)
