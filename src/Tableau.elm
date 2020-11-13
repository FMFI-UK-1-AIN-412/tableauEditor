module Tableau exposing (..)

import Formula exposing (Formula)
import Parser
import Formula.Signed exposing (Signed)
import Formula.Parser


type alias Tableau =
    { node : Node
    , ext : Extension
    }


type alias Node =
    { id : Int
    , value : String
    , reference : Ref
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
    { id = 1, value = "", reference = defRef, formula = Formula.Parser.parseSigned "", gui = defGUI }
