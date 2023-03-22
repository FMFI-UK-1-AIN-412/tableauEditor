module Tableau.Branch exposing
    ( Branch
    , canCheckCompleteness
    , fromZipper
    , isComplete
    , isOpen
    )


import Errors exposing (..)
import Formula as F exposing (Formula)
import Formula.Signed as SF exposing (Signed(..))
import Tableau exposing (..)
import Zipper exposing (Zipper)
import Parser
import Dict
import Set


type alias Branch = List (Signed Formula)


fromZipper : Zipper -> Result (List Parser.DeadEnd) Branch
fromZipper z =
    Result.map2 (::)
        (Zipper.zNode z).formula
        (case z of
            (_, []) ->
                Ok []

            _ ->
                fromZipper (Zipper.up z))


type alias FormulaOccurences =
    { true : List Formula
    , false : List Formula
    }


defFormulaOccurences =
    { true = []
    , false = []
    }


updateFormulaOccurences : Signed Formula -> Maybe FormulaOccurences -> Maybe FormulaOccurences
updateFormulaOccurences sf mbis =
    let
        bis = Maybe.withDefault defFormulaOccurences mbis
    in
        Just <| case sf of
            T f ->
                { bis | true = f :: bis.true }

            F f ->
                { bis | false = f :: bis.false }


branchDict b =
    List.foldl
        (\sf d ->
            Dict.update
                (sf |> SF.getFormula |> F.toString)
                (updateFormulaOccurences sf)
                d)
        Dict.empty b


isOpen : Branch -> Bool
isOpen b =
    branchDict b
        |> Dict.values
        |> List.all
            (\fos -> List.isEmpty fos.false || List.isEmpty fos.true)


canCheckCompleteness : Branch -> Bool
canCheckCompleteness b =
    b |> List.all (\sf -> SF.isAlpha sf || SF.isBeta sf)


testByType sf =
    if SF.isAlpha sf then
        List.all
    else
        List.any


isComplete : Branch -> Bool
isComplete b =
    let
        bSet = Set.fromList (List.map SF.toString b)
    in
        b |> List.all
            (\sf ->
                SF.subformulas sf
                    |> List.map SF.toString
                    |> testByType sf (\subf -> Set.member subf bSet))
