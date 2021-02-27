module Helpers.Exporting.Json.Encode exposing (encode, jsonTableau)

import Json.Encode exposing (..)
import Tableau exposing (Extension(..), ExtType(..), GUI, Node, Tableau)


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


jsonSubstitution : { a | var : String, term : String } -> Value
jsonSubstitution { term, var } =
    object
        [ ( "term", string term )
        , ( "var", string var )
        ]


encodeSubstitution : Tableau.Substitution -> List ( String, Value )
encodeSubstitution s =
    [ ( "substitution", jsonSubstitution s ) ]


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

        Unary Alpha t ->
            encodeUnaryRule tableau "alpha" t

        Binary Beta lt rt ->
            encodeBinaryRule tableau "beta" lt rt

        UnaryWithSubst Gamma t s ->
            encodeUnaryRuleWithSubst tableau "gamma" t s

        UnaryWithSubst Delta t s ->
            encodeUnaryRuleWithSubst tableau "delta" t s

        Unary Refl t ->
            encodeUnaryRule tableau "refl" t

        Unary Leibnitz t ->
            encodeUnaryRule tableau "leibnitz" t

        Unary MP t ->
            encodeUnaryRule tableau "mp" t
        
        Unary MT t ->
            encodeUnaryRule tableau "mt" t

        Binary Cut lt rt ->
            encodeBinaryRule tableau "cut" lt rt

        Unary HS t ->
            encodeUnaryRule tableau "hs" t

        Unary DS t ->
            encodeUnaryRule tableau "ds" t

        Unary NCS t ->
            encodeUnaryRule tableau "ncs" t

        _ ->
            encodeUnaryRule tableau "unknown" tableau


jsonTableau : Tableau -> Value
jsonTableau t =
    object <| jsonTblList t


encode : Int -> Tableau -> String
encode ind t =
    Json.Encode.encode ind (jsonTableau t) ++ "\n"



{- vim: set sw=2 ts=2 sts=2 et : -}
