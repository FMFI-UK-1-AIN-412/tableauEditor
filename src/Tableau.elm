module Tableau exposing (..)

import Formula
import Parser


type alias Tableau =
    { node : Node
    , ext : Extension
    }


type alias Node =
    { id : Int
    , value : String
    , reference : Ref
    , formula : Result (List Parser.DeadEnd) (Formula.Signed Formula.Formula)
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
    { id = 1, value = "", reference = defRef, formula = Formula.parseSigned "", gui = defGUI }
