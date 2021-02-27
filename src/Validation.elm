module Validation exposing (..)

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
import Validation.Common exposing (..)
import Validation.Rules.Alpha
import Validation.Rules.Beta
import Validation.Rules.Delta
import Validation.Rules.Gamma
import Validation.Rules.Leibnitz
import Validation.Rules.Reflexivity
import Zipper
import Validation.Rules.ModusPonens
import Validation.Rules.ModusTolens
import Validation.Rules.Cut
import Validation.Rules.HS
import Validation.Rules.DS
import Validation.Rules.NCS



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
                (isValidRef "First close" r1 z)
                (isValidRef "Second close" r2 z)

        _ ->
            Ok z


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


isCorrectRule :
    ( Tableau, Zipper.BreadCrumbs )
    -> Result (List Problem) ( Tableau, Zipper.BreadCrumbs )
isCorrectRule (( t, bs ) as z) =
    case bs of
        (Zipper.UnaryCrumb Alpha _) :: _ ->
            case t.node.references |> List.isEmpty of
                True ->
                    -- This is a premise
                    Ok z

                False ->
                    Validation.Rules.Alpha.validate z

        (Zipper.BinaryLeftCrumb Beta _ _ ) :: _ ->
            Validation.Rules.Beta.validateLeft z

        (Zipper.BinaryRightCrumb Beta _ _ ) :: _ ->
            Validation.Rules.Beta.validateRight z

        (Zipper.UnaryCrumbWithSubst Gamma _ _  ) :: _ ->
            Validation.Rules.Gamma.validate z

        (Zipper.UnaryCrumbWithSubst Delta _ _ ) :: _ ->
            Validation.Rules.Delta.validate z

        (Zipper.UnaryCrumb Refl _ ) :: _ ->
            Validation.Rules.Reflexivity.validate z

        (Zipper.UnaryCrumb Leibnitz _ ) :: _ ->
            Validation.Rules.Leibnitz.validate z
            
        (Zipper.UnaryCrumb MP _ ) :: _ ->
            Validation.Rules.ModusPonens.validate z
            
        (Zipper.UnaryCrumb MT _ ) :: _ ->
            Validation.Rules.ModusTolens.validate z
            
        (Zipper.BinaryLeftCrumb Cut _ _) :: _ ->
            Validation.Rules.Cut.validateLeft z

        (Zipper.BinaryRightCrumb Cut _ _) :: _ ->
            Validation.Rules.Cut.validateRight z
            
        (Zipper.UnaryCrumb HS _) :: _ ->
            Validation.Rules.HS.validate z
            
        (Zipper.UnaryCrumb DS _) :: _ ->
            Validation.Rules.DS.validate z
            
        (Zipper.UnaryCrumb NCS _) :: _ ->
            Validation.Rules.NCS.validate z

        _ ->    
            Ok z



{- vim: set sw=2 ts=2 sts=2 et :s -}
