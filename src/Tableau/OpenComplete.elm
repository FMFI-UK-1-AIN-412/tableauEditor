module Tableau.OpenComplete exposing (branch, isOpen, isComplete)

import Errors exposing (..)
import Formula exposing (Formula, Signed(..))
import Tableau exposing (..)
import Parser
import Dict
import Set


branch : Zipper -> Result (List Parser.DeadEnd) (List (Signed Formula))
branch z =
    Result.map2 (::)
        (zFormula z)
        (case z of
            (_, []) ->
                Ok []

            _ ->
                branch (up z))


type alias BranchEntry =
    { true : List Formula
    , false : List Formula
    }


defBranchEntry =
    { true = []
    , false = []
    }


updateBranchEntry : Signed Formula -> Maybe BranchEntry -> Maybe BranchEntry
updateBranchEntry sf mbe =
    let
        be = Maybe.withDefault defBranchEntry mbe
    in
        Just <| case sf of
            T f ->
                { be | true = f :: be.true }

            F f ->
                { be | false = f :: be.false }


branchDict b =
    List.foldl
        (\sf d ->
            Dict.update
                (sf |> Formula.signedGetFormula |> Formula.strFormula)
                (updateBranchEntry sf)
                d)
        Dict.empty b


isOpen : (List (Signed Formula)) -> Bool
isOpen b =
    branchDict b
        |> Dict.values
        |> List.all (\be -> List.isEmpty be.false || List.isEmpty be.true)


testByType sf =
    case Formula.signedType sf of
        Formula.Alpha ->
            List.all

        Formula.Beta ->
            List.any

isComplete : (List (Signed Formula)) -> Bool
isComplete b =
    let
        bSet = Set.fromList (List.map Formula.strSigned b)
    in
        b |> List.all
            (\sf ->
                Formula.signedSubformulas sf
                    |> List.map Formula.strSigned
                    |> testByType sf (\subf -> Set.member subf bSet))
