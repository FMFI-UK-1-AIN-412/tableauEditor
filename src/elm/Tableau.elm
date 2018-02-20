module Tableau exposing (..)


type alias Tableau =
    { node : Node, ext : Extension }


type alias Node =
    { id : Int, value : String, reference : Ref }


type alias Ref =
    { str : String, up : Maybe Int }


type Extension
    = Open
    | Closed Ref Ref
    | Alpha Tableau
    | Beta Tableau Tableau


defRef =
    { str = "", up = Nothing }


defNode =
    { id = 1, value = "", reference = defRef }
