module Tableau exposing (..)

import Formula
import Parser
import Dict


type alias Tableau =
    { node : Node, ext : Extension }


type alias Node =
    { id : Int, value : String, reference : Ref, formula : Result Parser.Error (Formula.Signed Formula.Formula) }


type alias Ref =
    { str : String, up : Maybe Int }


type alias Substitution =
    { what : String, forWhat : String }


type Extension
    = Open
    | Closed Ref Ref
    | Alpha Tableau
    | Beta Tableau Tableau
    | Gamma Tableau Substitution -- -> premenna a term su stringy
    | Delta Tableau Substitution


defSubstitution =
    { what = "", forWhat = "" }


defRef =
    { str = "", up = Nothing }


defNode =
    { id = 1, value = "", reference = defRef, formula = Formula.parseSigned "" }
