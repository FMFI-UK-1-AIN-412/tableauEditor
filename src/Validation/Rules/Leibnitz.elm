module Validation.Rules.Leibnitz exposing (validate, commonTemplate, checkSubsts)

import Dict
import Formula exposing (Formula(..))
import Formula.Signed exposing (Signed(..))
import Term exposing (Term(..))
import Tableau exposing (..)
import Zipper
import Validation.Common exposing(..)
import Helpers.Rules exposing (signedMap)


templateVar = Var "[]"


differentStructureError = 
    Err "The 2nd referenced formula and current formula have different structure"


differentSignError = Err "The 2nd referenced formula and current formula have different sign"


isEquality : Signed Formula -> Bool
isEquality f =
    case f of
        T (EqAtom lt rt) ->
            True

        _ ->
            False


leftEqTerm : Signed Formula -> Term
leftEqTerm f = 
    case f of 
        T (EqAtom lt rt) ->
            lt
        _ ->
            Fun "default"[]


rightEqTerm : Signed Formula -> Term
rightEqTerm f = 
    case f of 
        T (EqAtom lt rt) ->
            rt
        _ ->
            Fun "default"[]


replaceInTerms : List Term -> List Term -> List Term
replaceInTerms refTerms currentTerms =
    case refTerms of
        [] ->
            case currentTerms of
                [] ->
                    []

                _ ->
                    List.map (always templateVar) currentTerms
        
        refTerm :: rts ->
            case currentTerms of
                [] ->
                    List.map (always templateVar) refTerms

                currentTerm :: cts ->
                    (replaceInTerm refTerm currentTerm) ::
                    (replaceInTerms rts cts) 


replaceInTerm : Term -> Term -> Term
replaceInTerm refTerm currentTerm =
    case refTerm of
        Var refStr ->
            case currentTerm of
                Var currentStr ->
                    if refStr /= currentStr then
                        templateVar
                    else
                        Var refStr
                
                Fun _ _ ->
                    templateVar

        Fun refStr refTerms ->
            case currentTerm of
                Fun currentStr currentTerms ->
                    if refStr /= currentStr || List.length refTerms /= List.length currentTerms then
                        templateVar
                    else
                        Fun refStr (replaceInTerms refTerms currentTerms)
                
                Var _ ->
                    templateVar


replaceInFormula : Formula -> Formula -> Result String (Formula)
replaceInFormula refF currentF =
    case refF of
        PredAtom refStr refTerms ->
            case currentF of
                PredAtom currentStr currentTerms ->
                    Ok (PredAtom refStr
                    (replaceInTerms refTerms currentTerms))

                _ ->
                    differentStructureError

        EqAtom refLt refRt ->
            case currentF of
                EqAtom currentLt currentRt ->
                    Ok (EqAtom
                    (replaceInTerm refLt currentLt)
                    (replaceInTerm refRt currentRt))

                _ ->
                    differentStructureError

        Neg refSf ->
            case currentF of
                Neg currentSf ->
                    Result.map (\f -> Neg f)
                    (replaceInFormula refSf currentSf) 

                _ ->
                    differentStructureError

        Conj refSf1 refSf2 ->
            case currentF of
                Conj currentSf1 currentSf2 ->
                    Result.map2 (\f1 f2 -> Conj f1 f2)
                    (replaceInFormula refSf1 currentSf1)
                    (replaceInFormula refSf2 currentSf2)

                _ ->
                    differentStructureError

        Disj refSf1 refSf2 ->
            case currentF of
                Disj currentSf1 currentSf2 ->
                    Result.map2 (\f1 f2 -> Disj f1 f2)
                    (replaceInFormula refSf1 currentSf1)
                    (replaceInFormula refSf2 currentSf2)

                _ ->
                    differentStructureError

        Impl refSf1 refSf2 ->
            case currentF of
                Impl currentSf1 currentSf2 ->
                    Result.map2 (\f1 f2 -> Impl f1 f2)
                    (replaceInFormula refSf1 currentSf1)
                    (replaceInFormula refSf2 currentSf2)

                _ ->
                    differentStructureError

        ForAll refX refSf ->
            case currentF of
                ForAll currentX currentSf ->
                    Result.map (\f -> ForAll refX f)
                    (replaceInFormula refSf currentSf) 

                _ ->
                    differentStructureError

        Exists refX refSf ->
            case currentF of
                Exists currentX currentSf ->
                    Result.map (\f -> Exists refX f)
                    (replaceInFormula refSf currentSf) 

                _ ->
                    differentStructureError

        _ ->
            Err "wrong formula type"


commonTemplate : (Signed Formula) -> (Signed Formula) -> Result String (Signed Formula)
commonTemplate refF currentF =
    case refF of 
        T f ->
            case currentF of 
                F f1 ->
                    differentSignError

                T f1 ->
                    replaceInFormula f (Formula.Signed.getFormula currentF)
                    |> Result.map (\a -> T a)
        
        F f ->
            case currentF of 
                F f1 ->
                    replaceInFormula f (Formula.Signed.getFormula currentF)
                    |> Result.map (\a -> F a)

                T f1 ->
                    differentSignError
            

applyFunToSigned : (a -> Result e a) -> Signed a -> Result e (Signed a)
applyFunToSigned function sf =
    case sf of
        T formula ->
            (function formula) |> Result.map (\f -> T f) 

        F formula ->
            (function formula) |> Result.map (\f -> F f) 


mapNotSubstitutableError : String -> Zipper.Zipper -> List Problem
mapNotSubstitutableError err z = 
    (semanticsProblem z 
    (String.replace "[]"  ("[] in 2nd referenced formula") err))


checkSubst : Term.Substitution -> Result (List Problem) (Signed Formula) -> Signed Formula
 -> Zipper.Zipper -> Result (List Problem) Zipper.Zipper
checkSubst σ replaced currentF z = 
    case replaced of 
        Err problem -> 
            Err problem
        Ok repl ->  
            (applyFunToSigned (Formula.substitute σ) repl) 
            |> Result.mapError (\err -> mapNotSubstitutableError err z)
            |> Result.andThen 
                (checkPredicate (\f -> f == currentF) 
                (semanticsProblem z "Substitution invalid"))
            |> Result.map (always z)


checkSubsts : (Signed Formula) -> (Signed Formula) -> Zipper.Zipper
 -> Result (List Problem) Zipper.Zipper
checkSubsts refEq refF z =
    let 
        lt = leftEqTerm refEq 

        rt = rightEqTerm refEq 

        currentF = (Zipper.zNode z).formula |> Result.withDefault (T (PredAtom "default" []))

        replaced = commonTemplate refF currentF |> Result.mapError (\err -> (semanticsProblem z err))

        σ1 = Dict.fromList [("[]", lt)]

        σ2 = Dict.fromList [("[]", rt)]
    in 
    z |> (checkSubst σ1 replaced refF)
    |> Result.andThen (checkSubst σ2 replaced currentF)


validate : Zipper.Zipper -> Result (List Problem) Zipper.Zipper
validate z =
    z |> checkPredicate (hasNumberOfRefs 2)
        (semanticsProblem z "Leibnitz must have 2 references")
    |> Result.andThen (checkReffedFormula "first" (Zipper.zFirstRef z))
    |> Result.andThen
        (checkPredicate isEquality
            (semanticsProblem z "first referenced formula is not equality")
        )
    |> Result.andThen (\_ -> (checkReffedFormula "second" (Zipper.zSecondRef z)) z)
    |> Result.andThen (\_ ->
        (checkSubsts 
        (getReffedSignedFormula Zipper.zFirstRef z 
            |> Result.withDefault (T (PredAtom "default" []))) 
        (getReffedSignedFormula Zipper.zSecondRef z 
            |> Result.withDefault (T (PredAtom "default" []))))
        z)
    |> Result.map (always z)