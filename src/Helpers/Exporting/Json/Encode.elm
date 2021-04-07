module Helpers.Exporting.Json.Encode exposing (encode, jsonTableau)

import Config exposing (Config)
import Json.Encode exposing (..)
import Tableau exposing (..)


jsonRef : { a | str : String } -> Value
jsonRef r =
    string r.str


jsonNode : Tableau.Node -> Value
jsonNode { id, value, references, gui } =
    object
        [ ( "id", int id )
        , ( "value", string value )
        , ( "references", list jsonRef references )
        ]


jsonNodeList : Node -> List ( String, Value )
jsonNodeList n =
    [ ( "node", jsonNode n ) ]


jsonSubstitution : Tableau.Substitution -> Value
jsonSubstitution { str, parsedSubst } =
    object
        [ ( "str", string str )
        ]


encodeSubstitution : Tableau.Substitution -> List ( String, Value )
encodeSubstitution s =
    [ ( "substitution", jsonSubstitution s ) ]


jsonConfig : Config -> List ( String, Value )
jsonConfig config =
    [ ( "config", string <| Config.toString config )
    ]


encodeUnaryRule : Tableau -> String -> Tableau -> List ( String, Value )
encodeUnaryRule tableau extType subTableau =
    [ ( "type", string extType ) ]
        ++ jsonNodeList tableau.node
        ++ [ ( "child", jsonTableau subTableau ) ]


encodeUnaryRuleWithSubst : Tableau -> String -> Tableau -> Tableau.Substitution -> List ( String, Value )
encodeUnaryRuleWithSubst tableau extType subTableau subst =
    encodeUnaryRule tableau extType subTableau
        ++ encodeSubstitution subst


encodeBinaryRule : Tableau -> String -> Tableau -> Tableau -> List ( String, Value )
encodeBinaryRule tableau extType lt rt =
    [ ( "type", string extType ) ]
        ++ jsonNodeList tableau.node
        ++ [ ( "leftChild", jsonTableau lt ), ( "rightChild", jsonTableau rt ) ]


jsonTblList : Tableau -> List ( String, Value )
jsonTblList tableau =
    case tableau.ext of
        Open ->
            [ ( "type", string "open" ) ]
                ++ jsonNodeList tableau.node

        Closed r1 r2 ->
            [ ( "type", string "closed" ) ]
                ++ jsonNodeList tableau.node
                ++ [ ( "closed"
                     , list jsonRef [ r1, r2 ]
                     )
                   ]

        Unary extType t ->
            encodeUnaryRule tableau (unaryExtTypeJsonStr extType) t

        UnaryWithSubst extType t s ->
            encodeUnaryRuleWithSubst tableau (unaryWithSubstExtTypeJsonStr extType) t s

        Binary extType lt rt ->
            encodeBinaryRule tableau (binaryExtTypeJsonStr extType) lt rt


jsonTableau : Tableau -> Value
jsonTableau t =
    object <| jsonTblList t


jsonTableauAndConfig : Config -> Tableau -> Value
jsonTableauAndConfig config t =
    object <| jsonTblList t ++ jsonConfig config


encode : Int -> Config -> Tableau -> String
encode ind config t =
    Json.Encode.encode ind (jsonTableauAndConfig config t) ++ "\n"



{- vim: set sw=2 ts=2 sts=2 et : -}
