module Validation.Common exposing (..)

import Dict
import Errors
import Formula exposing (Formula(..))
import Formula.Parser
import Formula.Signed exposing (Signed(..))
import Helpers.Parser
import Parser
import Set
import Tableau exposing (..)
import Term exposing (Term(..))
import Zipper


type ProblemType
    = Syntax
    | Semantics


type alias Problem =
    { typ : ProblemType
    , msg : String
    , zip : Zipper.Zipper
    }


type RuleErrorType = 
    RefFormulasErr
    | CurrentFormulaErr


type alias RuleError = 
    { typ : RuleErrorType
    , msg : String
    }   


syntaxProblem : Zipper.Zipper -> String -> List Problem
syntaxProblem z s =
    [ { typ = Syntax, msg = s, zip = z } ]


semanticsProblem : Zipper.Zipper -> String -> List Problem
semanticsProblem z s =
    [ { typ = Semantics, msg = s, zip = z } ]


checkFormula :
    String
    -> Zipper.Zipper
    -> Result (List Problem) (Signed Formula)
checkFormula str z =
    z
        |> Zipper.zNode
        |> .formula
        |> Result.mapError (\_ -> semanticsProblem z (str ++ " is invalid."))


checkReffedFormula : String -> Tableau.Ref -> Zipper.Zipper -> Result (List Problem) (Signed Formula)
checkReffedFormula str r z =
    z
        |> Zipper.getReffed r
        |> Result.fromMaybe (semanticsProblem z (str ++ " reference is invalid."))
        |> Result.andThen (checkFormula (str ++ " referenced formula"))


getReffedSignedFormula : (Zipper.Zipper -> Ref) -> Zipper.Zipper -> Result (List Problem) (Signed Formula)
getReffedSignedFormula extractRef z =
    case Zipper.getReffed (extractRef z) z of
        Just rz ->
            case rz |> Zipper.zNode |> .formula of
                Ok sf ->
                    Ok sf

                Err _ ->
                    Err (syntaxProblem z "reffed formula incorrectly parsed")

        Nothing ->
            Err (semanticsProblem z "no reffed formula")


makeSemantic : List Problem -> List Problem
makeSemantic =
    List.map (\p -> { p | typ = Semantics })


resultFromBool : a -> x -> Bool -> Result x a
resultFromBool a x b =
    if b then
        Ok a

    else
        Err x



{- Check the value using a predicate.
   Pass the value along if predicate returns true,
   return the given error otherwise
-}


checkPredicate : (a -> Bool) -> x -> a -> Result x a
checkPredicate pred x a =
    if pred a then
        Ok a

    else
        Err x


second : a1 -> a2 -> a2
second =
    \a b -> Tuple.second ( a, b )


always4 : a -> b -> c -> d -> e -> a
always4 r _ _ _ _ =
    r


always3 : a -> b -> c -> d -> a
always3 r _ _ _ =
    r


always2 : a -> b -> c -> a
always2 r _ _ =
    r


getParsedSubst : Zipper.Zipper -> Term.Substitution
getParsedSubst z =
    z |> Zipper.zSubstitution 
    |> Maybe.withDefault Tableau.defSubstitution
    |> .parsedSubst 
    |> Result.withDefault (Dict.fromList [])


getTermsToString : Zipper.Zipper -> String
getTermsToString z = 
    z |> getParsedSubst |> Dict.values 
    |> (\ts -> (String.join "," (List.map Term.toString ts)))


getVarsToString : Zipper.Zipper -> String
getVarsToString z = 
    (z |> getParsedSubst |> Dict.keys |> String.join ",")


substitutionIsValid : Term.Substitution -> Signed Formula -> Signed Formula -> Bool
substitutionIsValid substitution new original =
    let
        applyToSigned :
            (Term.Substitution -> Formula -> Result String Formula)
            -> Term.Substitution
            -> Signed Formula
            -> Signed Formula
        applyToSigned function subst sf =
            case sf of
                T formula ->
                    T (checkSubstitution (function subst formula))

                F formula ->
                    F (checkSubstitution (function subst formula))

        checkSubstitution : Result String Formula -> Formula
        checkSubstitution r =
            case r of
                Ok f ->
                    f

                Err msg ->
                    PredAtom "default" []
    in
    new == applyToSigned Formula.removeQuantifierAndSubstitute substitution original


isSubstituable : Term.Substitution -> Signed Formula -> Signed Formula -> Bool
isSubstituable substitution new original =
    let
        removeSign : Term.Substitution -> Signed Formula -> Bool
        removeSign s sf =
            case sf of
                T formula ->
                    removeQuantifierAndSubstitute s formula

                F formula ->
                    removeQuantifierAndSubstitute s formula

        removeQuantifierAndSubstitute : Term.Substitution -> Formula -> Bool
        removeQuantifierAndSubstitute s f =
            case f of
                Formula.ForAll v subf ->
                    if List.member v (Dict.keys s) then
                        removeQuantifierAndSubstitute s subf

                    else
                        trySubs substitution f

                Formula.Exists v subf ->
                    if List.member v (Dict.keys s) then
                        removeQuantifierAndSubstitute s subf

                    else
                        trySubs substitution f

                _ ->
                    trySubs substitution f

        trySubs : Term.Substitution -> Formula -> Bool
        trySubs s f =
            case Formula.substitute s f of
                Ok _ ->
                    True

                Err msg ->
                    False
    in
    removeSign substitution original




getReffedId : (Zipper.Zipper -> Ref) -> Zipper.Zipper -> String
getReffedId extractRef z =
    String.fromInt
        (Zipper.getReffed (extractRef z) z
            |> Maybe.map (Zipper.zNode >> .id)
            |> Maybe.withDefault 0
        )


hasNumberOfRefs : Int -> Zipper.Zipper -> Bool
hasNumberOfRefs n z =
    List.length (Zipper.zNode z).references == n
    

numberOfSubstPairs : Zipper.Zipper -> Int
numberOfSubstPairs z = 
    case Zipper.zSubstitution (Zipper.up z) of
        Just subst ->
            case subst.parsedSubst of 
                Ok parsed ->
                    Dict.size parsed
                Err _ ->
                    0
        Nothing ->
            0


{- If one of the errors is CurrentFormulaErr, it will be returned. Returns the first error otherwise -}
ruleErrorToShow: RuleError -> RuleError -> RuleError
ruleErrorToShow e1 e2 = 
    case e1.typ of 
        CurrentFormulaErr ->
            e1

        RefFormulasErr ->
            case e2.typ of
                CurrentFormulaErr ->
                    e2
                _ ->
                    e1


{- Returns the first Ok Result. If both are Err, returns the one with CurrentFormulaErr -}
betterOutcome : Result RuleError value -> Result RuleError value -> Result RuleError value
betterOutcome r1 r2 = 
        case r1 of
            Ok val1 ->
               Ok val1
            Err err1 ->
                case r2 of
                    Ok val2 ->
                        Ok val2
                    Err err2 ->
                        Err (ruleErrorToShow err1 err2)


validate2RefUnaryRule : String -> (Signed Formula -> Signed Formula -> Zipper.Zipper -> Result (List Problem) Zipper.Zipper) -> Zipper.Zipper -> Result (List Problem) Zipper.Zipper
validate2RefUnaryRule ruleName check z =
    z
        |> checkPredicate (hasNumberOfRefs 2)
            (semanticsProblem z (ruleName ++ " rule must have 2 references"))
        |> Result.andThen (checkReffedFormula "first" (Zipper.zFirstRef z))
        |> Result.andThen (\_ -> checkReffedFormula "second" (Zipper.zSecondRef z) z)
        |> Result.andThen
            (\_ ->
                check
                    (getReffedSignedFormula Zipper.zFirstRef z
                        |> Result.withDefault (T (PredAtom "default" []))
                    )
                    (getReffedSignedFormula Zipper.zSecondRef z
                        |> Result.withDefault (T (PredAtom "default" []))
                    )
                    z
            )
        |> Result.map (always z) 


tryBothFormulaOrders: (Signed Formula -> Signed Formula -> Signed Formula -> Result RuleError String) -> Signed Formula -> Signed Formula ->  Zipper.Zipper -> Result (List Problem) Zipper.Zipper
tryBothFormulaOrders checkFormulaOrder sf1 sf2 z = 
    let
        currentF = (Zipper.zNode z).formula |> Result.withDefault (T (PredAtom "default" []))
    in
        betterOutcome (checkFormulaOrder sf1 sf2 currentF) (checkFormulaOrder sf2 sf1 currentF)
        |> Result.mapError (\err -> (semanticsProblem z err.msg))
        |> Result.map (always z)


tryBothOrdersAndStructures : (Signed Formula -> Signed Formula -> Signed Formula -> Result RuleError String) -> (Signed Formula -> Signed Formula -> Signed Formula -> Result RuleError String) -> Signed Formula -> Signed Formula -> Zipper.Zipper -> Result (List Problem) Zipper.Zipper
tryBothOrdersAndStructures struct1 struct2 sf1 sf2 z =
    let
        currentF =
            (Zipper.zNode z).formula |> Result.withDefault (T (PredAtom "default" []))

        firstOption =
            betterOutcome (struct1 sf1 sf2 currentF) (struct1 sf2 sf1 currentF)

        secondOption =
            betterOutcome (struct2 sf1 sf2 currentF) (struct2 sf2 sf1 currentF)
    in
    betterOutcome firstOption secondOption
        |> Result.mapError (\err -> semanticsProblem z err.msg)
        |> Result.map (always z)


validateBinary :
    String ->
    (Signed Formula -> Zipper.Zipper -> Result (List Problem) (List (Signed Formula))) ->
    Zipper.Zipper
    -> Zipper.Zipper
    -> Result (List Problem) Zipper.Zipper
validateBinary ruleName getChildren this other =
    let
        ft =
            this |> checkFormula "Formula"

        fo =
            other |> checkFormula ("The other " ++ ruleName ++ " subformula")

        children =
            ft
                |> Result.map List.singleton
                |> Result.map2 (::) fo
                |> Result.map (List.sortBy Formula.Signed.toString)

        -- This is a hack, but defining an ordering on formulas...
        reffed =
            this
                |> checkPredicate (hasNumberOfRefs 1)
                    (semanticsProblem this ("Each " ++ ruleName ++ " formula must have 1 reference"))
                |> Result.andThen (checkReffedFormula "" (Zipper.zFirstRef this))
                |> Result.map (always this)
                |> Result.andThen (\z -> getReffedSignedFormula Zipper.zFirstRef z)
                |> Result.andThen
                    (\f -> getChildren f this)
                |> Result.map (List.sortBy Formula.Signed.toString)
    in
    Errors.merge2 (==) children reffed
        |> Result.andThen (resultFromBool this (semanticsProblem this ("Wrong " ++ ruleName ++ " subformulas.")))
        |> Errors.merge2 (always2 this) (childrenHaveSameRef ruleName this other)


childrenHaveSameRef :
    String 
    -> Zipper.Zipper
    -> Zipper.Zipper
    -> Result (List Problem) Zipper.Zipper
childrenHaveSameRef ruleName this other =
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
            (resultFromBool this (semanticsProblem this (ruleName ++ " references are not the same")))


validateLeft : (Zipper.Zipper
    -> Zipper.Zipper
    -> Result (List Problem) Zipper.Zipper) 
    -> Zipper.Zipper -> Result (List Problem) Zipper.Zipper
validateLeft validate z =
    validate z (z |> Zipper.up |> Zipper.right)


validateRight : (Zipper.Zipper
    -> Zipper.Zipper
    -> Result (List Problem) Zipper.Zipper) 
    -> Zipper.Zipper -> Result (List Problem) Zipper.Zipper
validateRight validate z =
    validate z (z |> Zipper.up |> Zipper.left)