module Tableau exposing (..)

import Dict
import Formula
import Parser


type alias Tableau =
    { node : Node, ext : Extension }


type alias Node =
    { id : Int
    , value : String
    , reference : Ref
    , formula : Result Parser.Error (Formula.Signed Formula.Formula)
    , gui : GUI
    }


type alias GUI =
    { controlsShown : Bool }


type alias Ref =
    { str : String, up : Maybe Int }


type alias Substitution =
    { what : String, forWhat : String }


type Extension
    = Open
    | Closed Ref Ref
    | Alpha Tableau
    | Beta Tableau Tableau
    | Gamma Tableau Substitution
    | Delta Tableau Substitution


defSubstitution : Substitution
defSubstitution =
    { what = "", forWhat = "" }


defRef : { str : String, up : Maybe a }
defRef =
    { str = "", up = Nothing }


defGUI : { controlsShown : Bool }
defGUI =
    { controlsShown = False }


defNode :
    { formula : Result Parser.Error (Formula.Signed Formula.Formula)
    , id : number
    , reference : { str : String, up : Maybe a }
    , value : String
    , gui : GUI
    }
defNode =
    { id = 1, value = "", reference = defRef, formula = Formula.parseSigned "", gui = defGUI }
