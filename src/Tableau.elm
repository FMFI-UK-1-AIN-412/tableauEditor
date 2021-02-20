module Tableau exposing (..)

import Formula exposing (Formula)
import Formula.Parser
import Formula.Signed exposing (Signed)
import Parser


type alias Tableau =
    { node : Node
    , ext : Extension
    }


type alias Node =
    { id : Int
    , value : String
    , references : List Ref
    , formula : Result (List Parser.DeadEnd) (Signed Formula)
    , gui : GUI
    }


type alias GUI =
    { controlsShown : Bool }


type alias Ref =
    { str : String
    , up : Maybe Int
    }


type alias Substitution =
    { term : String, var : String }


type Extension
    = Open
    | Closed Ref Ref
    | Alpha Tableau
    | Beta Tableau Tableau
    | Gamma Tableau Substitution
    | Delta Tableau Substitution
    | Refl Tableau
    | Leibnitz Tableau
    | MP Tableau
    | MT Tableau
    | Cut Tableau Tableau
    | HS Tableau
    | DS Tableau
    | NCS Tableau


defSubstitution : Substitution
defSubstitution =
    { term = "", var = "" }


defRef : { str : String, up : Maybe a }
defRef =
    { str = "", up = Nothing }


defGUI : { controlsShown : Bool }
defGUI =
    { controlsShown = True }


defNode : Node
defNode =
    { id = 1, value = "", references = [], formula = Formula.Parser.parseSigned "", gui = defGUI }


isEmpty : Tableau -> Bool
isEmpty t =
    t.node.value == "" && t.ext == Open


leftSubtree : Tableau -> Tableau
leftSubtree t =
    case t.ext of
        Open ->
            Tableau defNode Open

        Closed _ _ ->
            Tableau defNode Open

        Alpha subT ->
            subT

        Beta leftSubT _ ->
            leftSubT

        Gamma subT _ ->
            subT

        Delta subT _ ->
            subT

        Refl subT ->
            subT

        Leibnitz subT ->
            subT

        MP subT ->
            subT
        
        MT subT ->
            subT
        
        Cut leftSubT _ ->
            leftSubT

        HS subT ->
            subT

        DS subT ->
            subT

        NCS subT ->
            subT


rightSubtree : Tableau -> Tableau
rightSubtree t =
    case t.ext of
        Beta _ rt ->
            rt

        Cut _ rt ->
            rt

        _ ->
            Tableau defNode Open


strRefsToList : String -> List String
strRefsToList str =
    let
        lst =
            List.filter (\a -> a /= "") (String.split "," (String.replace " " "" str))
    in
    if String.right 1 str == "," then
        List.append lst [ "" ]

    else if String.left 1 str == "," then
        "" :: lst

    else
        lst


refsToString : List Ref -> String
refsToString lst =
    String.join "," (List.map (\r -> r.str) lst)
