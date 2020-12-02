module Validation.Leibnitz exposing (validateLeibnitzRule, replaceTermWithVar)

import Dict
import Formula exposing (Formula(..))
import Formula.Signed exposing (Signed(..))
import Term exposing (Term(..))
import Tableau exposing (..)
import Zipper
import Validation.Common exposing(..)


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


replaceInTerms : Term -> Term -> List Term -> List Term -> Zipper.Zipper -> List Term
replaceInTerms term var refTerms currentTerms z =
    case refTerms of
        [] ->
            []
        
        refTerm :: rts ->
            case currentTerms of
                [] ->
                    []

                currentTerm :: cts ->
                    (replaceInTerm term var refTerm currentTerm z) ::
                    (replaceInTerms term var rts cts z) 


replaceInTerm : Term -> Term -> Term -> Term -> Zipper.Zipper -> Term
replaceInTerm term var refTerm currentTerm z =
    if refTerm == term && refTerm /= currentTerm then  
        var
    else
        case refTerm of
            Var refStr ->
                Var refStr

            Fun refStr refTerms ->
                case currentTerm of
                    Fun currentStr currentTerms ->
                        if refStr == currentStr then
                            Fun refStr (replaceInTerms term var refTerms currentTerms z)
                        else
                            Fun refStr refTerms  --nema zmysel nahradzat vnutri funckie ked ma iny nazov
                    
                    Var currentStr ->
                        var


replaceInFormula : Term -> Term -> Formula -> Formula -> Zipper.Zipper -> 
 Result (List Problem) (Formula)
replaceInFormula term var refF currentF z =
    case refF of
        PredAtom refStr refTerms ->
            case currentF of
                PredAtom currentStr currentTerms ->
                    Ok (PredAtom refStr
                    (replaceInTerms term var refTerms currentTerms z))

                _ ->
                    Err (semanticsProblem z "...should only differ in terms")

        EqAtom refLt refRt ->
            case currentF of
                EqAtom currentLt currentRt ->
                    Ok (EqAtom
                    (replaceInTerm term var refLt currentLt z)
                    (replaceInTerm term var refRt currentRt z))

                _ ->
                    Err (semanticsProblem z "...should only differ in terms")

        Neg refSf ->
            case currentF of
                Neg currentSf ->
                    Result.map (\f -> Neg f)
                    (replaceInFormula term var refSf currentSf z) 

                _ ->
                    Err (semanticsProblem z "...should only differ in terms")

        Conj refSf1 refSf2 ->
            case currentF of
                Conj currentSf1 currentSf2 ->
                    Result.map2 (\f1 f2 -> Conj f1 f2)
                    (replaceInFormula term var refSf1 currentSf1 z)
                    (replaceInFormula term var refSf2 currentSf2 z)

                _ ->
                    Err (semanticsProblem z "...should only differ in terms")

        Disj refSf1 refSf2 ->
            case currentF of
                Disj currentSf1 currentSf2 ->
                    Result.map2 (\f1 f2 -> Disj f1 f2)
                    (replaceInFormula term var refSf1 currentSf1 z)
                    (replaceInFormula term var refSf2 currentSf2 z)

                _ ->
                    Err (semanticsProblem z "...should only differ in terms")

        Impl refSf1 refSf2 ->
            case currentF of
                Impl currentSf1 currentSf2 ->
                    Result.map2 (\f1 f2 -> Impl f1 f2)
                    (replaceInFormula term var refSf1 currentSf1 z)
                    (replaceInFormula term var refSf2 currentSf2 z)

                _ ->
                    Err (semanticsProblem z "...should only differ in terms")

        ForAll refX refSf ->
            case currentF of
                ForAll currentX currentSf ->
                    Result.map (\f -> ForAll refX f)
                    (replaceInFormula term var refSf currentSf z) 

                _ ->
                    Err (semanticsProblem z "...should only differ in terms")

        Exists refX refSf ->
            case currentF of
                Exists currentX currentSf ->
                    Result.map (\f -> Exists refX f)
                    (replaceInFormula term var refSf currentSf z) 

                _ ->
                    Err (semanticsProblem z "...should only differ in terms")

        _ ->
            Err (semanticsProblem z "wrong formula type")


replaceTermWithVar : Term -> Term -> Signed Formula -> Signed Formula -> Zipper.Zipper -> 
 Result (List Problem) (Signed Formula)
replaceTermWithVar term var refF currentF z =
    case refF of 
        T f ->
            replaceInFormula term var f (Formula.Signed.getFormula currentF) z
            |> Result.map (\a -> T a)
        
        F f ->
            replaceInFormula term var f (Formula.Signed.getFormula currentF) z
            |> Result.map (\a -> F a)


applyFunToSigned : (Term.Substitution -> Formula -> Result String Formula) -> Term.Substitution ->
 Signed Formula -> Signed Formula
applyFunToSigned function subst sf =
    case sf of
        T formula ->
            (function subst formula) |> Result.map (\f -> T f) 
            |> Result.withDefault (T (Formula.PredAtom "default" []))

        F formula ->
            (function subst formula) |> Result.map (\f -> F f) 
            |> Result.withDefault (T (Formula.PredAtom "default" []))


checkSubstitutable : Term.Substitution -> Result (List Problem) (Signed Formula) -> Zipper.Zipper
 -> Result (List Problem) Zipper.Zipper
checkSubstitutable σ replaced z = 
    case replaced of 
        Err problem -> 
            Err problem
        Ok f ->  
            Formula.substitute σ (Formula.Signed.getFormula f)
            |> Result.mapError (\err -> (semanticsProblem z err))
            |> Result.map (always z)


checkValidSubst : Term.Substitution -> Result (List Problem) (Signed Formula) -> Signed Formula
 ->Zipper.Zipper -> Result (List Problem) Zipper.Zipper
checkValidSubst σ replaced currentF z = 
    case replaced of 
        Err problem -> 
            Err problem
        Ok f ->  
            if applyFunToSigned Formula.substitute σ f == currentF then
                Ok z 
            else
                Err (semanticsProblem z "Substitution invalid")


checkSubsts : (Signed Formula) -> (Signed Formula) -> Zipper.Zipper
 -> Result (List Problem) Zipper.Zipper
checkSubsts refEq refF z =
    let 
        lt = leftEqTerm refEq 

        rt = rightEqTerm refEq 

        currentF = (Zipper.zNode z).formula |> Result.withDefault (T (PredAtom "default" []))

        replaced = replaceTermWithVar lt (Var "[]") refF currentF z

        σ1 = Dict.fromList [("[]", lt)]

        σ2 = Dict.fromList [("[]", rt)]
    in 
    z |> checkSubstitutable σ1 replaced
    |> Result.andThen (checkSubstitutable σ2 replaced)
    |> Result.andThen (checkValidSubst σ2 replaced currentF)


validateLeibnitzRule : Zipper.Zipper -> Result (List Problem) Zipper.Zipper
validateLeibnitzRule z =
    if List.length (Zipper.zNode z).references /= 2 then
        Err (semanticsProblem z "Leibnitz must have 2 references")
    else
        z |> checkReffedFormula "first" (Zipper.zFirstRef z)
        |> Result.andThen
            (checkPredicate isEquality
                (semanticsProblem z "first Referenced formula is not equality")
            )
        |> Result.andThen (\_ ->
            (checkIsPointingOnSelf
                (isPointingOnSelf Zipper.zFirstRef)
                (semanticsProblem z "Leibnitz can not be pointing on itself")
                z
            ))
        |> Result.andThen (checkReffedFormula "second" (Zipper.zSecondRef z))
        |> Result.andThen (\_ ->
            (checkIsPointingOnSelf
                (isPointingOnSelf Zipper.zSecondRef)
                (semanticsProblem z "Leibnitz can not be pointing on itself")
                z
            ))
        |> Result.andThen (\_ ->
            (checkSubsts 
            (getReffedSignedFormula Zipper.zFirstRef z 
                |> Result.withDefault (T (PredAtom "default" []))) 
            (getReffedSignedFormula Zipper.zSecondRef z 
                |> Result.withDefault (T (PredAtom "default" []))))
            z)
        |> Result.map (always z)