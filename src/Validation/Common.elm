module Validation.Common exposing (..)

import Dict
import Errors
import Formula exposing (Formula(..))
import Formula.Parser
import Formula.Signed exposing (Signed(..))
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


implicitSubst : Term.Substitution -> Zipper.Zipper -> Term.Substitution
implicitSubst subst z =
    unsubstitutedVars subst z |> List.map (\x -> ( Term.toString x, x )) |> Dict.fromList


getParsedSubst : Zipper.Zipper -> Term.Substitution
getParsedSubst z =
    (z |> Zipper.up)
        |> Zipper.zSubstitution
        |> Maybe.withDefault Tableau.defSubstitution
        |> .parsedSubst
        |> Result.withDefault (Dict.fromList [])
        |> (\parsedS -> Dict.union parsedS (implicitSubst parsedS z))


getTermsToString : Zipper.Zipper -> String
getTermsToString z =
    z
        |> getParsedSubst
        |> Dict.values
        |> (\ts -> String.join "," (List.map Term.toString ts))


getVarsToString : Zipper.Zipper -> String
getVarsToString z =
    z |> getParsedSubst |> Dict.keys |> String.join ","


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
    z |> getParsedSubst |> Dict.size


checkFormulas err f1 f2 getNewFormula z =
    let
        newFormula =
            z |> checkFormula "Formula"

        correctNewFormula =
            getNewFormula f1 f2
                |> Result.mapError (semanticsProblem z)
    in
    Result.map2 (==) newFormula correctNewFormula
        |> Result.andThen (resultFromBool z (semanticsProblem z err))


validate2RefUnary : String -> (Signed Formula -> Signed Formula -> Zipper.Zipper -> Result (List Problem) Zipper.Zipper) -> Zipper.Zipper -> Result (List Problem) Zipper.Zipper
validate2RefUnary ruleName check z =
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


validate2RefBinary :
    String
    -> (Signed Formula -> Zipper.Zipper -> Result (List Problem) (List (Signed Formula)))
    -> Zipper.Zipper
    -> Zipper.Zipper
    -> Result (List Problem) Zipper.Zipper
validate2RefBinary ruleName getChildren this other =
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


validateLeft :
    (Zipper.Zipper
     -> Zipper.Zipper
     -> Result (List Problem) Zipper.Zipper
    )
    -> Zipper.Zipper
    -> Result (List Problem) Zipper.Zipper
validateLeft validate z =
    validate z (z |> Zipper.up |> Zipper.right)


validateRight :
    (Zipper.Zipper
     -> Zipper.Zipper
     -> Result (List Problem) Zipper.Zipper
    )
    -> Zipper.Zipper
    -> Result (List Problem) Zipper.Zipper
validateRight validate z =
    validate z (z |> Zipper.up |> Zipper.left)


unsubstitutedVars : Term.Substitution -> Zipper.Zipper -> List Term
unsubstitutedVars subst z =
    let
        newF =
            checkFormula "Formula" z |> Result.map Formula.Signed.getFormula

        refF =
            getReffedSignedFormula Zipper.zFirstRef z |> Result.map Formula.Signed.getFormula

        removedQuants =
            Result.map2 numOfRemovedQuants refF newF
    in
    removedQuants
        |> Result.andThen
            (\n ->
                Result.map (\f -> getUnsubstitutedVars [] subst n f) refF
            )
        |> Result.withDefault []


getUnsubstitutedVars : List Term -> Term.Substitution -> Int -> Formula -> List Term
getUnsubstitutedVars vars subst n f =
    varsToBeSubstituted vars n f
        |> List.filter (\v -> not <| List.member (Term.toString v) (Dict.keys subst))


varsToBeSubstituted : List Term -> Int -> Formula -> List Term
varsToBeSubstituted vars n f =
    if n == 0 then
        vars

    else
        case f of
            ForAll v subf ->
                varsToBeSubstituted (Var v :: vars) (n - 1) subf

            Exists v subf ->
                varsToBeSubstituted (Var v :: vars) (n - 1) subf

            _ ->
                vars


startWithSameQuant : Formula -> Formula -> Bool
startWithSameQuant f1 f2 =
    case ( f1, f2 ) of
        ( ForAll _ _, ForAll _ _ ) ->
            True

        ( Exists _ _, Exists _ _ ) ->
            True

        _ ->
            False


haveSameSign : Signed Formula -> Signed Formula -> Bool
haveSameSign f1 f2 =
    case ( f1, f2 ) of
        ( T _, T _ ) ->
            True

        ( F _, F _ ) ->
            True

        _ ->
            False


numOfRemovedQuants : Formula -> Formula -> Int
numOfRemovedQuants refF newF =
    if not (startWithSameQuant refF newF) then
        countLeadingQuantifiers refF

    else
        Basics.max 0
            (countLeadingQuantifiers refF
                - countLeadingQuantifiers newF
            )


countExistQuantifiers : Int -> Formula -> Int
countExistQuantifiers count f =
    case f of
        Formula.Exists x subf ->
            countExistQuantifiers (count + 1) subf

        _ ->
            count


countForAllQuantifiers : Int -> Formula -> Int
countForAllQuantifiers count f =
    case f of
        Formula.ForAll x subf ->
            countForAllQuantifiers (count + 1) subf

        _ ->
            count


countLeadingQuantifiers : Formula -> Int
countLeadingQuantifiers f =
    case f of
        Formula.ForAll x subf ->
            countForAllQuantifiers 0 f

        Formula.Exists x subf ->
            countExistQuantifiers 0 f

        _ ->
            0


removeNQuants : Int -> Formula -> Formula
removeNQuants n f =
    if n == 0 then
        f

    else
        case f of
            ForAll _ subf ->
                removeNQuants (n - 1) subf

            Exists _ subf ->
                removeNQuants (n - 1) subf

            _ ->
                f


forbiddenVarsErrStr lst =
    "The variable"
        ++ pluralFromList " '" "s '" lst
        ++ String.join "," lst
        ++ "' can't be substituted for"


checkSubstitutedVars : List Term -> Term.Substitution -> Zipper.Zipper -> Result (List Problem) Zipper.Zipper
checkSubstitutedVars allowedVars subst z =
    let
        allowed =
            allowedVars |> List.map Term.toString

        forbidden =
            subst |> Dict.keys |> List.filter (\v -> not (List.member v allowed))
    in
    case forbidden of
        [] ->
            Ok z

        lst ->
            Err <| semanticsProblem z (forbiddenVarsErrStr lst)


unaryWithSubstCheck : String -> Signed Formula -> Signed Formula -> Term.Substitution -> Zipper.Zipper -> Result (List Problem) Zipper.Zipper
unaryWithSubstCheck ruleName refF newF subst z =
    let
        referenced =
            Formula.Signed.getFormula refF

        new =
            Formula.Signed.getFormula newF

        removedQuantsCount =
            numOfRemovedQuants referenced new

        allowedVars =
            varsToBeSubstituted [] removedQuantsCount referenced
    in
    checkSubstitutedVars allowedVars subst z
        |> Result.andThen
            (\_ ->
                removeNQuants removedQuantsCount referenced
                    |> Formula.substitute subst
                    |> Result.mapError (\str -> semanticsProblem z str)
                    |> Result.map (\f -> f == new && haveSameSign refF newF)
                    |> Result.andThen
                        (\b ->
                            resultFromBool z
                                (semanticsProblem z ("Formula was not created by using the " ++ ruleName ++ " rule"))
                                b
                        )
            )


notVarsErrStr lst =
    "The term"
        ++ pluralFromList " '" "s '" lst
        ++ String.join "', '" (List.map Term.toString lst)
        ++ "' must be "
        ++ pluralFromList "a variable" "variables" lst


similarAboveErrStr lst =
    "The variable"
        ++ pluralFromList " '" "s '" lst
        ++ String.join "," lst
        ++ pluralFromList "' was" "' were" lst
        ++ " located above as free"


pluralFromList singular plural lst =
    case lst of
        _ :: _ :: _ -> plural
        _ -> singular


isFunction : Term -> Bool
isFunction term =
    case term of
        Var _ ->
            False

        Fun _ _ ->
            True


checkNewVariables : Zipper.Zipper -> Result (List Problem) Zipper.Zipper
checkNewVariables z =
    let
        parsedS =
            getParsedSubst z

        terms =
            Dict.values parsedS

        termsToString =
            List.map Term.toString terms
    in
    case List.filter isFunction terms of
        [] ->
            case List.filter (\var -> isSimilarAbove var (z |> Zipper.up)) termsToString of
                [] ->
                    areDistinct termsToString z

                vars ->
                    Err (semanticsProblem z (similarAboveErrStr vars))

        functions ->
            Err (semanticsProblem z (notVarsErrStr functions))


areDistinct vars z = 
    List.length vars == Set.size (Set.fromList vars)
    |> resultFromBool z (semanticsProblem z "Substituted variables must be distinct")


isSimilarAbove : String -> Zipper.Zipper -> Bool
isSimilarAbove var z =
    let
        parsedF =
            z |> Zipper.zNode |> .value |> Formula.Parser.parseSigned
    in
    case parsedF of
        Ok parsed ->
            Set.member var (Formula.free (parsed |> Formula.Signed.getFormula))
                || (z |> Zipper.up)
                /= z
                && isSimilarAbove var (z |> Zipper.up)

        Err _ ->
            (z |> Zipper.up) /= z && isSimilarAbove var (z |> Zipper.up)
