module Tableau exposing (..)

import Formula
import Parser


type alias Tableau =
    { node : Node, ext : Extension }


type alias Node =
    { id : Int, value : String, reference : Ref, formula : Result Parser.Error (Formula.Signed Formula.Formula) }


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
    { id = 1, value = "", reference = defRef, formula = Formula.parseSigned "" }
