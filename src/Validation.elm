module Validation exposing (..)

import Config exposing (Config)
import Dict
import Errors
import Formula exposing (Formula(..), toString)
import Formula.Signed exposing (Signed(..))
import Helpers.Parser
import Parser
import Set
import Tableau exposing (..)
import Term exposing (Term(..))
import Validation.Common exposing (..)
import Validation.Rules.Alpha
import Validation.Rules.Beta
import Validation.Rules.Cut
import Validation.Rules.DS
import Validation.Rules.Delta
import Validation.Rules.DeltaStar
import Validation.Rules.ECDF
import Validation.Rules.ECDT
import Validation.Rules.ESFF
import Validation.Rules.ESFT
import Validation.Rules.ESTF
import Validation.Rules.ESTT
import Validation.Rules.Gamma
import Validation.Rules.GammaStar
import Validation.Rules.HS
import Validation.Rules.Leibnitz
import Validation.Rules.ModusPonens
import Validation.Rules.ModusTolens
import Validation.Rules.NCS
import Validation.Rules.Reflexivity
import Zipper



-- error : x -> Result x a -> x
-- error def r =
--     case r of
--         Err x ->
--             x
--         Ok _ ->
--             def
-- (<++) : ( List Problem, Zipper.Zipper ) -> (Zipper.Zipper -> List Problem) -> ( List Problem, Zipper.Zipper )
-- (<++) ( lp, z ) f =
--     ( lp ++ f z, z )
-- (<++?) : ( List Problem, Zipper.Zipper ) -> (Zipper.Zipper -> Result (List Problem) a) -> ( List Problem, Zipper.Zipper )
-- (<++?) ( lp, z ) f =
--     ( lp ++ error [] (f z), z )


isCorrectTableau : Config -> Zipper.Zipper -> Result (List Problem) Zipper.Zipper
isCorrectTableau config z =
    Errors.merge2 (always2 z)
        (isCorrectNode config z)
        (List.foldl
            (Errors.merge2 (always2 z))
            (Ok z)
            (List.map (isCorrectTableau config) (Zipper.children z))
        )


isValidNode : Zipper.Zipper -> Result (List Problem) Zipper.Zipper
isValidNode z =
    Errors.merge4 (always4 z)
        (isValidFormula z)
        (isValidNodeRef z)
        (isValidSubstitution z)
        (areValidCloseRefs z)


isCorrectNode : Config -> Zipper.Zipper -> Result (List Problem) Zipper.Zipper
isCorrectNode config z =
    isValidNode z
        |> Result.andThen
            (\_ ->
                Errors.merge2 second
                    (isCorrectRule config z)
                    (areCorrectCloseRefs z)
            )


{-| Just for the formula dislplay -- don't check the ref for syntax
-}
isCorrectFormula : Config -> Zipper.Zipper -> Result (List Problem) Zipper.Zipper
isCorrectFormula config z =
    isValidFormula z
        |> Result.andThen (isCorrectRule config)


isValidFormula : Zipper.Zipper -> Result (List Problem) Zipper.Zipper
isValidFormula z =
    z |> Zipper.zNode |> .formula |> Result.mapError (parseProblem z) |> Result.map (always z)


isValidSubstitution : Zipper.Zipper -> Result (List Problem) Zipper.Zipper
isValidSubstitution z =
    if Zipper.up z == z then
        Ok z

    else
        case Zipper.zSubstitution (Zipper.up z) of
            Just subst ->
                subst
                    |> .parsedSubst
                    |> Result.mapError (\_ -> syntaxProblem z "Wrong form of substitution")
                    |> Result.map (\parsedS -> Dict.union parsedS (implicitSubst parsedS z))
                    |> Result.map (always z)

            Nothing ->
                Ok z


isValidNodeRef : Zipper.Zipper -> Result (List Problem) Zipper.Zipper
isValidNodeRef z =
    case List.length (Zipper.zNode z).references of
        0 ->
            Ok z

        1 ->
            isValidRef "The" (Zipper.zFirstRef z) z

        2 ->
            Errors.merge2 (always2 z)
                (isValidRef "The first" (Zipper.zFirstRef z) z)
                (isValidRef "The second" (Zipper.zSecondRef z) z)

        _ ->
            Err (syntaxProblem z "There are too many references.")


areValidCloseRefs : Zipper.Zipper -> Result (List Problem) Zipper.Zipper
areValidCloseRefs z =
    case (Zipper.zTableau z).ext of
        Closed r1 r2 ->
            Errors.merge2 (always2 z)
                (isValidCloseRef "First close" r1 z)
                (isValidCloseRef "Second close" r2 z)

        _ ->
            Ok z


isValidCloseRef : String -> Ref -> Zipper.Zipper -> Result (List Problem) Zipper.Zipper
isValidCloseRef str r z =
    r.up
        |> Result.fromMaybe (syntaxProblem z (str ++ " reference is invalid."))
        |> Result.map (always z)


isValidRef : String -> Ref -> Zipper.Zipper -> Result (List Problem) Zipper.Zipper
isValidRef str r z =
    r.up
        |> Result.fromMaybe (syntaxProblem z (str ++ " reference is invalid."))
        |> Result.andThen
            (checkPredicate (\up -> up /= 0)
                (semanticsProblem z (str ++ " reference is pointing on this formula"))
            )
        |> Result.map (always z)


areCorrectCloseRefs :
    Zipper.Zipper
    -> Result (List Problem) Zipper.Zipper
areCorrectCloseRefs z =
    case (Zipper.zTableau z).ext of
        Closed r1 r2 ->
            areCloseRefsComplementary r1 r2 z |> Result.map (always z)

        _ ->
            Ok z


parseProblem : Zipper.Zipper -> List Parser.DeadEnd -> List Problem
parseProblem z =
    Helpers.Parser.deadEndsToString >> syntaxProblem z


validateFormula : Zipper.Zipper -> Result (List Problem) (Signed Formula)
validateFormula z =
    z |> Zipper.zNode |> .formula |> Result.mapError (parseProblem z)


validateRef : String -> Ref -> Zipper.Zipper -> List Problem
validateRef str r z =
    case r.up of
        Nothing ->
            syntaxProblem z str

        _ ->
            []


validateNodeRef : Zipper.Zipper -> List Problem
validateNodeRef z =
    case List.length (Zipper.zNode z).references of
        0 ->
            []

        1 ->
            validateRef "Invalid reference" (Zipper.zFirstRef z) z

        2 ->
            validateRef "Invalid first reference" (Zipper.zFirstRef z) z
                ++ validateRef "Invalid second reference" (Zipper.zSecondRef z) z

        _ ->
            syntaxProblem z "There are too many references."


areCloseRefsComplementary :
    Ref
    -> Ref
    -> Zipper.Zipper
    -> Result (List Problem) Zipper.Zipper
areCloseRefsComplementary r1 r2 z =
    Errors.merge2 Formula.Signed.isComplementary
        (checkReffedFormula "First close" r1 z)
        (checkReffedFormula "Second close" r2 z)
        |> Result.andThen (resultFromBool z (semanticsProblem z "Closing formulas are not complementary."))


validateUnary : UnaryExtType -> Zipper.Zipper -> Result (List Problem) Zipper.Zipper
validateUnary extType =
    case extType of
        Alpha ->
            Validation.Rules.Alpha.validate

        Refl ->
            Validation.Rules.Reflexivity.validate

        Leibnitz ->
            Validation.Rules.Leibnitz.validate

        MP ->
            Validation.Rules.ModusPonens.validate

        MT ->
            Validation.Rules.ModusTolens.validate

        HS ->
            Validation.Rules.HS.validate

        DS ->
            Validation.Rules.DS.validate

        NCS ->
            Validation.Rules.NCS.validate

        ESFF ->
            Validation.Rules.ESFF.validate

        ESFT ->
            Validation.Rules.ESFT.validate

        ESTF ->
            Validation.Rules.ESTF.validate

        ESTT ->
            Validation.Rules.ESTT.validate


validateUnaryWithSubst : UnaryWithSubstExtType -> Zipper.Zipper -> Result (List Problem) ( Tableau, Zipper.BreadCrumbs )
validateUnaryWithSubst extType =
    case extType of
        Gamma ->
            Validation.Rules.Gamma.validate

        Delta ->
            Validation.Rules.Delta.validate

        GammaStar ->
            Validation.Rules.GammaStar.validate

        DeltaStar ->
            Validation.Rules.DeltaStar.validate


validateBinary : BinaryExtType -> Zipper.Zipper -> Zipper.Zipper -> Result (List Problem) Zipper.Zipper
validateBinary extType =
    case extType of
        Beta ->
            Validation.Rules.Beta.validate

        Cut ->
            Validation.Rules.Cut.validate

        ECDF ->
            Validation.Rules.ECDF.validate

        ECDT ->
            Validation.Rules.ECDT.validate


validateRule rule validator config =
    if Set.member rule <| Config.getRuleSet config then
        validator

    else
        \z -> Err <| semanticsProblem z <| rule ++ " rule is forbidden in current configuration"


isCorrectRule :
    Config.Config
    -> ( Tableau, Zipper.BreadCrumbs )
    -> Result (List Problem) ( Tableau, Zipper.BreadCrumbs )
isCorrectRule config (( t, bs ) as z) =
    case bs of
        (Zipper.UnaryCrumb Alpha _) :: _ ->
            case t.node.references |> List.isEmpty of
                True ->
                    -- This is a premise
                    Ok z

                False ->
                    validateRule (unaryExtTypeToString Alpha) (validateUnary Alpha) config z

        (Zipper.UnaryCrumb extType _) :: _ ->
            validateRule (unaryExtTypeToString extType) (validateUnary extType) config z

        (Zipper.UnaryCrumbWithSubst extType _ _) :: _ ->
            validateRule (unaryWithSubstExtTypeToString extType) (validateUnaryWithSubst extType) config z

        (Zipper.BinaryLeftCrumb extType _ _) :: _ ->
            validateRule (binaryExtTypeToString extType) (validateLeft (validateBinary extType)) config z

        (Zipper.BinaryRightCrumb extType _ _) :: _ ->
            validateRule (binaryExtTypeToString extType) (validateRight (validateBinary extType)) config z

        [] ->
            Ok z



{- vim: set sw=2 ts=2 sts=2 et :s -}
