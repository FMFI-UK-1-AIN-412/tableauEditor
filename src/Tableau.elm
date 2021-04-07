module Tableau exposing (..)

import Dict
import Formula exposing (Formula)
import Formula.Parser
import Formula.Signed exposing (Signed)
import Parser
import Term


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
    { str : String
    , parsedSubst : Result (List Parser.DeadEnd) Term.Substitution
    }


type UnaryExtType
    = Alpha
    | Refl
    | Leibnitz
    | MP
    | MT
    | HS
    | DS
    | NCS
    | ESFF
    | ESFT
    | ESTF
    | ESTT


type UnaryWithSubstExtType
    = Gamma
    | Delta
    | GammaStar
    | DeltaStar


type BinaryExtType
    = Beta
    | Cut
    | ECDF
    | ECDT


type Extension
    = Open
    | Closed Ref Ref
    | Unary UnaryExtType Tableau
    | UnaryWithSubst UnaryWithSubstExtType Tableau Substitution
    | Binary BinaryExtType Tableau Tableau


defSubstitution : Substitution
defSubstitution =
    { str = "", parsedSubst = Ok (Dict.fromList []) }


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

        Unary _ subT ->
            subT

        UnaryWithSubst _ subT _ ->
            subT

        Binary _ leftSubT _ ->
            leftSubT


rightSubtree : Tableau -> Tableau
rightSubtree t =
    case t.ext of
        Binary _ _ rightSubT ->
            rightSubT

        _ ->
            Tableau defNode Open


strRefsToList : String -> List String
strRefsToList str =
    if String.isEmpty str then 
        [] 
    else
        (String.split "," str)


refsToString : List Ref -> String
refsToString lst =
    String.join "," (List.map (\r -> r.str) lst)


unaryExtTypeToString : UnaryExtType -> String
unaryExtTypeToString extType =
    case extType of
        Alpha ->
            "α"

        Refl ->
            "Reflexivity"

        Leibnitz ->
            "Leibnitz"

        MP ->
            "MP"

        MT ->
            "MT"

        HS ->
            "HS"

        DS ->
            "DS"

        NCS ->
            "NCS"

        ESFF ->
            "ESFF"

        ESFT ->
            "ESFT"

        ESTF ->
            "ESTF"

        ESTT ->
            "ESTT"


unaryWithSubstExtTypeToString : UnaryWithSubstExtType -> String
unaryWithSubstExtTypeToString extType =
    case extType of
        Gamma ->
            "γ"

        Delta ->
            "δ"

        GammaStar ->
            "γ*"

        DeltaStar ->
            "δ*"


binaryExtTypeToString : BinaryExtType -> String
binaryExtTypeToString extType =
    case extType of
        Beta ->
            "β"

        Cut ->
            "Cut"

        ECDF ->
            "ECDF"

        ECDT ->
            "ECDT"


unaryExtTypeJsonStr : UnaryExtType -> String
unaryExtTypeJsonStr extType =
    case extType of
        Alpha ->
            "alpha"

        Refl ->
            "refl"

        Leibnitz ->
            "leibnitz"

        MP ->
            "mp"

        MT ->
            "mt"

        HS ->
            "hs"

        DS ->
            "ds"

        NCS ->
            "ncs"

        ESFF ->
            "esff"

        ESFT ->
            "esft"

        ESTF ->
            "estf"

        ESTT ->
            "estt"


unaryWithSubstExtTypeJsonStr : UnaryWithSubstExtType -> String
unaryWithSubstExtTypeJsonStr extType =
    case extType of
        Gamma ->
            "gamma"

        Delta ->
            "delta"

        GammaStar ->
            "gammaStar"

        DeltaStar ->
            "deltaStar"


binaryExtTypeJsonStr : BinaryExtType -> String
binaryExtTypeJsonStr extType =
    case extType of
        Beta ->
            "beta"

        Cut ->
            "cut"

        ECDF ->
            "ecdf"

        ECDT ->
            "ecdt"
