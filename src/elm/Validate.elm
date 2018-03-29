module Validate exposing (..)

import Dict
import Errors
import Formula
import Parser
import Set
import Tableau exposing (..)
import Zipper


type ProblemType
    = Syntax
    | Semantics


type alias Problem =
    { typ : ProblemType
    , msg : String
    , zip : Zipper.Zipper
    }


syntaxProblem : a -> b -> List { msg : b, typ : ProblemType, zip : a }
syntaxProblem z s =
    [ { typ = Syntax, msg = s, zip = z } ]


semanticsProblem : a -> b -> List { msg : b, typ : ProblemType, zip : a }
semanticsProblem z s =
    [ { typ = Semantics, msg = s, zip = z } ]


error : x -> Result x a -> x
error def r =
    case r of
        Err x ->
            x

        Ok _ ->
            def


(<++) : ( List Problem, Zipper.Zipper ) -> (Zipper.Zipper -> List Problem) -> ( List Problem, Zipper.Zipper )
(<++) ( lp, z ) f =
    ( lp ++ f z, z )


(<++?) : ( List Problem, Zipper.Zipper ) -> (Zipper.Zipper -> Result (List Problem) a) -> ( List Problem, Zipper.Zipper )
(<++?) ( lp, z ) f =
    ( lp ++ error [] (f z), z )


second : a1 -> a2 -> a2
second =
    curry Tuple.second


always3 : a -> b -> c -> d -> a
always3 r _ _ _ =
    r


always2 : a -> b -> c -> a
always2 r _ _ =
    r


isCorrectTableau : Zipper.Zipper -> Result (List Problem) Zipper.Zipper
isCorrectTableau z =
    Errors.merge2 (always2 z)
        (isCorrectNode z)
        (List.foldl
            (Errors.merge2 (always2 z))
            (Ok z)
            (List.map isCorrectTableau (Zipper.children z))
        )


isValidNode : Zipper.Zipper -> Result (List Problem) Zipper.Zipper
isValidNode z =
    Errors.merge3 (always3 z)
        (isValidFormula z)
        (isValidNodeRef z)
        (areValidCloseRefs z)


isCorrectNode : Zipper.Zipper -> Result (List Problem) Zipper.Zipper
isCorrectNode z =
    isValidNode z
        |> Result.andThen
            (\_ ->
                Errors.merge2 second
                    (isCorrectRule z)
                    (areCorrectCloseRefs z)
            )


{-| Just for the formula dislplay -- don't check the ref for syntax
-}
isCorrectFormula : Zipper.Zipper -> Result (List Problem) Zipper.Zipper
isCorrectFormula z =
    isValidFormula z
        |> Result.andThen isCorrectRule


isValidFormula : Zipper.Zipper -> Result (List Problem) Zipper.Zipper
isValidFormula z =
    z |> Zipper.zNode |> .formula |> Result.mapError (parseProblem z) |> Result.map (always z)


isValidNodeRef : Zipper.Zipper -> Result (List { msg : String, typ : ProblemType, zip : Zipper.Zipper }) Zipper.Zipper
isValidNodeRef z =
    isValidRef "The" (Zipper.zNode z).reference z


areValidCloseRefs : Zipper.Zipper -> Result (List { msg : String, typ : ProblemType, zip : Zipper.Zipper }) Zipper.Zipper
areValidCloseRefs z =
    case (Zipper.zTableau z).ext of
        Closed r1 r2 ->
            Errors.merge2 (always2 z)
                (isValidRef "First close" r1 z)
                (isValidRef "Second close" r2 z)

        _ ->
            Ok z


isValidRef : String -> { a | up : Maybe b } -> c -> Result (List { msg : String, typ : ProblemType, zip : c }) c
isValidRef str r z =
    r.up
        |> Result.fromMaybe (syntaxProblem z (str ++ " reference is invalid."))
        |> Result.map (always z)


areCorrectCloseRefs :
    Zipper.Zipper
    -> Result (List { msg : String, typ : ProblemType, zip : Zipper.Zipper }) Zipper.Zipper
areCorrectCloseRefs z =
    case (Zipper.zTableau z).ext of
        Closed r1 r2 ->
            areCloseRefsComplementary r1 r2 z |> Result.map (always z)

        _ ->
            Ok z


parseProblem : Zipper.Zipper -> Parser.Error -> List Problem
parseProblem z =
    Formula.errorString >> syntaxProblem z


validateFormula : Zipper.Zipper -> Result (List Problem) (Formula.Signed Formula.Formula)
validateFormula z =
    z |> Zipper.zNode |> .formula |> Result.mapError (parseProblem z)


validateReffedFormula : Zipper.Zipper -> Result (List Problem) (Formula.Signed Formula.Formula)
validateReffedFormula z =
    z |> Zipper.zNode |> .formula |> Result.mapError (\e -> semanticsProblem z "Referenced formula is invalid")


validateRef : b -> { c | up : Maybe a } -> d -> List { msg : b, typ : ProblemType, zip : d }
validateRef str r z =
    case r.up of
        Nothing ->
            syntaxProblem z str

        _ ->
            []


validateNodeRef : Zipper.Zipper -> List { msg : String, typ : ProblemType, zip : Zipper.Zipper }
validateNodeRef z =
    validateRef "Invalid reference" (Zipper.zNode z).reference z


checkFormula :
    String
    -> Zipper.Zipper
    -> Result (List { msg : String, typ : ProblemType, zip : Zipper.Zipper }) (Formula.Signed Formula.Formula)
checkFormula str z =
    z
        |> Zipper.zNode
        |> .formula
        |> Result.mapError (\_ -> semanticsProblem z (str ++ " is invalid."))


checkReffedFormula : String -> Tableau.Ref -> Zipper.Zipper -> Result (List Problem) (Formula.Signed Formula.Formula)
checkReffedFormula str r z =
    z
        |> Zipper.getReffed r
        |> Result.fromMaybe (semanticsProblem z (str ++ " reference is invalid."))
        |> Result.andThen (checkFormula (str ++ " referenced formula"))


areCloseRefsComplementary :
    Ref
    -> Ref
    -> Zipper.Zipper
    -> Result (List { msg : String, typ : ProblemType, zip : Zipper.Zipper }) Zipper.Zipper
areCloseRefsComplementary r1 r2 z =
    Errors.merge2 Formula.isSignedComplementary
        (checkReffedFormula "First close" r1 z)
        (checkReffedFormula "Second close" r2 z)
        |> Result.andThen (resultFromBool z (semanticsProblem z "Closing formulas are not complementary."))


isCorrectRule :
    ( Tableau, Zipper.BreadCrumbs )
    -> Result (List { msg : String, typ : ProblemType, zip : Zipper.Zipper }) ( Tableau, Zipper.BreadCrumbs )
isCorrectRule (( t, bs ) as z) =
    case t.node.reference.up of
        Just 0 ->
            Ok z

        -- This is a premise
        _ ->
            case bs of
                (Zipper.AlphaCrumb _) :: _ ->
                    validateAlphaRule z

                (Zipper.BetaLeftCrumb _ _) :: _ ->
                    validateBetaRuleLeft z

                (Zipper.BetaRightCrumb _ _) :: _ ->
                    validateBetaRuleRight z

                (Zipper.GammaCrumb _ _) :: _ ->
                    validateGammaRule z

                (Zipper.DeltaCrumb _ _) :: _ ->
                    validateDeltaRule z

                [] ->
                    Ok z



-- Top of the tableau, this must be a premise


makeSemantic : List { b | typ : a } -> List { b | typ : ProblemType }
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


validateAlphaRule : Zipper.Zipper -> Result (List Problem) Zipper.Zipper
validateAlphaRule z =
    Zipper.getReffed (Zipper.zNode z).reference z
        |> Result.fromMaybe (semanticsProblem z "Invalid reference.")
        |> Result.andThen validateReffedFormula
        |> Result.andThen
            (checkPredicate Formula.isAlpha
                (semanticsProblem z "Referenced formula is not α")
            )
        |> Result.map2 (,) (checkFormula "Formula" z)
        |> Result.andThen
            (checkPredicate (uncurry Formula.isSignedSubformulaOf)
                (semanticsProblem z
                    ("Is not an α-subformula of ("
                        ++ toString (Zipper.getReffed (Zipper.zNode z).reference z |> Maybe.map (Zipper.zNode >> .id) |> Maybe.withDefault 0)
                        ++ ")."
                    )
                )
            )
        |> Result.map (always z)


validateBetaRuleLeft : Zipper.Zipper -> Result (List { msg : String, typ : ProblemType, zip : Zipper.Zipper }) ( Tableau, Zipper.BreadCrumbs )
validateBetaRuleLeft z =
    validateBeta z (z |> Zipper.up |> Zipper.right)


validateBetaRuleRight : Zipper.Zipper -> Result (List { msg : String, typ : ProblemType, zip : Zipper.Zipper }) ( Tableau, Zipper.BreadCrumbs )
validateBetaRuleRight z =
    validateBeta z (z |> Zipper.up |> Zipper.left)


validateBeta :
    Zipper.Zipper
    -> Zipper.Zipper
    -> Result (List { msg : String, typ : ProblemType, zip : Zipper.Zipper }) Zipper.Zipper
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
                |> Result.map (List.sortBy Formula.strSigned)

        -- This is a hack, but defining an ordering on formulas...
        reffed =
            this
                |> Zipper.getReffed (Zipper.zNode this).reference
                |> Result.fromMaybe (semanticsProblem this "Invalid reference")
                |> Result.andThen validateReffedFormula
                |> Result.andThen
                    (checkPredicate Formula.isBeta
                        (semanticsProblem this "Referenced formula is not β")
                    )
                |> Result.map Formula.signedSubformulas
                |> Result.map (List.sortBy Formula.strSigned)
    in
    Errors.merge2 (==) children reffed
        |> Result.andThen (resultFromBool this (semanticsProblem this "Wrong β subformulas."))
        |> Errors.merge2 (always2 this) (betasHaveSameRef this other)


betasHaveSameRef :
    Zipper.Zipper
    -> Zipper.Zipper
    -> Result (List { msg : String, typ : ProblemType, zip : Zipper.Zipper }) Zipper.Zipper
betasHaveSameRef this other =
    let
        -- The invalid refs will be reported already
        getRef =
            Zipper.zNode >> .reference >> .up >> Result.fromMaybe []

        rt =
            getRef this

        ro =
            getRef other
    in
    Errors.merge2 (==) rt ro
        |> Result.andThen
            (resultFromBool this (semanticsProblem this "β references are not the same"))


isSimilarAbove : String -> Zipper.Zipper -> Bool
isSimilarAbove variable z =
    let
        maybeParsed =
            z |> Zipper.zNode |> .value |> Formula.parseSigned
    in
    case maybeParsed of
        Ok parsed ->
            Set.member variable (Formula.freeFormula (parsed |> Formula.signedGetFormula))
                || (if (z |> Zipper.up) == z then
                        False
                    else
                        isSimilarAbove variable (z |> Zipper.up)
                   )

        Err _ ->
            False


isNewVariableValid : String -> Zipper.Zipper -> Bool
isNewVariableValid variable z =
    case getTermFromResult (Formula.parseTerm variable) of
        Formula.Var s ->
            not (isSimilarAbove variable (z |> Zipper.up))

        Formula.Fun _ _ ->
            False


getTermFromResult : Result Parser.Error Formula.Term -> Formula.Term
getTermFromResult r =
    case r of
        Ok term ->
            term

        Err err ->
            Formula.Fun "default" []


makeS : Tableau.Substitution -> Formula.Substitution
makeS subs =
    let
        newTerm =
            subs.what |> Formula.parseTerm |> getTermFromResult
    in
    Dict.fromList [ ( subs.forWhat, newTerm ) ]


substitutionIsValid : Formula.Substitution -> Formula.Signed Formula.Formula -> Formula.Signed Formula.Formula -> Bool
substitutionIsValid substitution new original =
    let
        applyToSigned :
            (Formula.Substitution -> Formula.Formula -> Result String Formula.Formula)
            -> Formula.Substitution
            -> Formula.Signed Formula.Formula
            -> Formula.Signed Formula.Formula
        applyToSigned function substitution sf =
            case sf of
                Formula.T formula ->
                    Formula.T (checkSubstitution (function substitution formula))

                Formula.F formula ->
                    Formula.F (checkSubstitution (function substitution formula))

        checkSubstitution : Result String Formula.Formula -> Formula.Formula
        checkSubstitution r =
            case r of
                Ok f ->
                    f

                Err msg ->
                    Formula.Atom "default" []
    in
    new == applyToSigned Formula.removeQuantifierAndSubstitute substitution original


isSubstituable : Formula.Substitution -> Formula.Signed Formula.Formula -> Formula.Signed Formula.Formula -> Bool
isSubstituable s new original =
    let
        _ =
            Debug.log "is Substituable formula" original

        _ =
            Debug.log "substitution " s

        removeSign : Formula.Substitution -> Formula.Signed Formula.Formula -> Bool
        removeSign s sf =
            case sf of
                Formula.T formula ->
                    removeQuantifierAndSubstitute s formula

                Formula.F formula ->
                    removeQuantifierAndSubstitute s formula

        removeQuantifierAndSubstitute : Formula.Substitution -> Formula.Formula -> Bool
        removeQuantifierAndSubstitute substitution original =
            case original of
                Formula.ForAll s f ->
                    if List.member s (Dict.keys substitution) then
                        removeQuantifierAndSubstitute substitution f
                    else
                        trySubs substitution original

                Formula.Exists s f ->
                    if List.member s (Dict.keys substitution) then
                        removeQuantifierAndSubstitute substitution f
                    else
                        trySubs substitution original

                _ ->
                    trySubs substitution original

        trySubs : Formula.Substitution -> Formula.Formula -> Bool
        trySubs s f =
            let
                _ =
                    Debug.log "subs says" (toString (Formula.substFormula s f))
            in
            case Formula.substFormula s f of
                Ok f ->
                    True

                Err msg ->
                    False
    in
    removeSign s original


validateGammaRule :
    Zipper.Zipper
    -> Result (List { msg : String, typ : ProblemType, zip : Zipper.Zipper }) ( Tableau, Zipper.BreadCrumbs )
validateGammaRule z =
    Zipper.getReffed (Zipper.zNode z).reference z
        |> Result.fromMaybe (semanticsProblem z "Invalid reference.")
        |> Result.andThen validateReffedFormula
        |> Result.andThen
            (checkPredicate Formula.isGamma
                (semanticsProblem z "Referenced formula is not γ")
            )
        |> Result.map2 (,) (checkFormula "Formula" z)
        |> Result.andThen
            -- checking substituable
            (checkPredicate
                (uncurry
                    (isSubstituable
                        (z
                            |> Zipper.up
                            |> Zipper.zSubstitution
                            |> Maybe.map makeS
                            |> Maybe.withDefault (Dict.fromList [])
                        )
                    )
                )
                (semanticsProblem z
                    ("This is not substituable. Variable '"
                        ++ (z
                                |> Zipper.up
                                |> Zipper.zSubstitution
                                |> Maybe.map .what
                                |> Maybe.map
                                    (\s ->
                                        if s == "" then
                                            "_"
                                        else
                                            s
                                    )
                                |> Maybe.withDefault ""
                           )
                        ++ "' is bound in referrenced formula ("
                        ++ toString (Zipper.getReffed (Zipper.zNode z).reference z |> Maybe.map (Zipper.zNode >> .id) |> Maybe.withDefault 0)
                        ++ "). Choose another variable."
                    )
                )
            )
        |> Result.andThen
            -- checking valid substitution
            (checkPredicate
                (uncurry
                    (substitutionIsValid
                        (z
                            |> Zipper.up
                            |> Zipper.zSubstitution
                            |> Maybe.map makeS
                            |> Maybe.withDefault (Dict.fromList [])
                        )
                    )
                )
                (semanticsProblem z
                    ("This isn't valid γ-subformula created by substituting '"
                        ++ (z
                                |> Zipper.up
                                |> Zipper.zSubstitution
                                |> Maybe.map .what
                                |> Maybe.map
                                    (\s ->
                                        if s == "" then
                                            "_"
                                        else
                                            s
                                    )
                                |> Maybe.withDefault ""
                           )
                        ++ "' for '"
                        ++ (z
                                |> Zipper.up
                                |> Zipper.zSubstitution
                                |> Maybe.map .forWhat
                                |> Maybe.map
                                    (\s ->
                                        if s == "" then
                                            "_"
                                        else
                                            s
                                    )
                                |> Maybe.withDefault ""
                           )
                        ++ "' from ("
                        ++ toString (Zipper.getReffed (Zipper.zNode z).reference z |> Maybe.map (Zipper.zNode >> .id) |> Maybe.withDefault 0)
                        ++ ")."
                    )
                )
            )
        |> Result.map (always z)
        |> Result.andThen
            -- checking existing variable above + if new constant is variable or function
            (checkPredicate
                (isNewVariableValid
                    (z
                        |> Zipper.up
                        |> Zipper.zSubstitution
                        |> Maybe.map makeS
                        |> Maybe.withDefault (Dict.fromList [])
                        |> Dict.values
                        |> List.head
                        |> Maybe.withDefault (Formula.Var "default")
                        |> Formula.strTerm
                    )
                )
                (semanticsProblem z
                    ("Substituting variable '"
                        ++ (z
                                |> Zipper.up
                                |> Zipper.zSubstitution
                                |> Maybe.map .what
                                |> Maybe.map
                                    (\s ->
                                        if s == "" then
                                            "_"
                                        else
                                            s
                                    )
                                |> Maybe.withDefault ""
                           )
                        ++ "' was located above as free. Please choose another, not used yet. "
                        ++ "Make sure, your new variable is variable and not function."
                    )
                )
            )
        |> Result.map (always z)


validateDeltaRule :
    Zipper.Zipper
    -> Result (List { msg : String, typ : ProblemType, zip : Zipper.Zipper }) ( Tableau, Zipper.BreadCrumbs )
validateDeltaRule z =
    Zipper.getReffed (Zipper.zNode z).reference z
        |> Result.fromMaybe (semanticsProblem z "Invalid reference.")
        |> Result.andThen validateReffedFormula
        |> Result.andThen
            (checkPredicate Formula.isDelta
                (semanticsProblem z "Referenced formula is not γ")
            )
        |> Result.map2 (,) (checkFormula "Formula" z)
        |> Result.map (always z)



{- vim: set sw=2 ts=2 sts=2 et :s -}
