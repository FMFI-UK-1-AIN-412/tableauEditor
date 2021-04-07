module Helpers.Exporting.Json.Decode exposing (decode, tableau)

import Config exposing (Config)
import Dict
import Formula
import Formula.Parser
import Json.Decode exposing (..)
import Tableau exposing (Tableau)
import Zipper



{- unfortunately we need to parse the refs
   in a second pass
-}


mkRef : String -> Tableau.Ref
mkRef str =
    { str = str, up = Nothing }


ref : Decoder Tableau.Ref
ref =
    map mkRef string


closedRefs : Decoder ( Tableau.Ref, Tableau.Ref )
closedRefs =
    map2 (\a b -> ( a, b ))
        (index 0 ref)
        (index 1 ref)


node : Decoder Tableau.Node
node =
    map5
        Tableau.Node
        (field "id" int)
        (field "value" string)
        (field "references" (list ref))
        (map Formula.Parser.parseSigned (field "value" string))
        (succeed { controlsShown = False })


substitution : Decoder Tableau.Substitution
substitution =
    map2 Tableau.Substitution
        (field "str" string)
        (map Formula.Parser.parseSubstitution (field "str" string))


config : Decoder Config
config =
    map Config.fromString
        (field "config" string)


open : Decoder Tableau.Tableau
open =
    map2 Tableau.Tableau
        (field "node" node)
        (succeed Tableau.Open)


closed : Decoder Tableau.Tableau
closed =
    map2
        Tableau.Tableau
        (field "node" node)
        (map2 Tableau.Closed (map Tuple.first (field "closed" closedRefs)) (map Tuple.second (field "closed" closedRefs)))


unaryRule : Tableau.UnaryExtType -> Decoder Tableau.Tableau
unaryRule extType =
    map2
        Tableau.Tableau
        (field "node" node)
        (map (Tableau.Unary extType) (field "child" (Json.Decode.lazy (\_ -> tableau))))


unaryRuleWithSubst : Tableau.UnaryWithSubstExtType -> Decoder Tableau.Tableau
unaryRuleWithSubst extType =
    map2
        Tableau.Tableau
        (field "node" node)
        (map2 (Tableau.UnaryWithSubst extType) (field "child" (lazy (\_ -> tableau))) (field "substitution" substitution))


binaryRule : Tableau.BinaryExtType -> Decoder Tableau.Tableau
binaryRule extType =
    map2 Tableau.Tableau
        (field "node" node)
        (map2 (Tableau.Binary extType) (field "leftChild" (lazy (\_ -> tableau))) (field "rightChild" (lazy (\_ -> tableau))))


tblTypeDecoder : String -> Decoder Tableau.Tableau
tblTypeDecoder typ =
    case typ of
        "open" ->
            open

        "closed" ->
            closed

        "alpha" ->
            unaryRule Tableau.Alpha

        "beta" ->
            binaryRule Tableau.Beta

        "gamma" ->
            unaryRuleWithSubst Tableau.Gamma

        "delta" ->
            unaryRuleWithSubst Tableau.Delta

        "refl" ->
            unaryRule Tableau.Refl

        "leibnitz" ->
            unaryRule Tableau.Leibnitz

        "mp" ->
            unaryRule Tableau.MP

        "mt" ->
            unaryRule Tableau.MT

        "cut" ->
            binaryRule Tableau.Cut

        "hs" ->
            unaryRule Tableau.HS

        "ds" ->
            unaryRule Tableau.DS

        "ncs" ->
            unaryRule Tableau.NCS

        "ecdf" ->
            binaryRule Tableau.ECDF

        "ecdt" ->
            binaryRule Tableau.ECDT

        "esff" ->
            unaryRule Tableau.ESFF

        "esft" ->
            unaryRule Tableau.ESFT

        "estf" ->
            unaryRule Tableau.ESTF

        "estt" ->
            unaryRule Tableau.ESTT

        "gammaStar" ->
            unaryRuleWithSubst Tableau.GammaStar

        "deltaStar" ->
            unaryRuleWithSubst Tableau.DeltaStar

        _ ->
            fail ("'" ++ typ ++ "' is not a correct tableau node type")



-- I have no idea why this also needs to be lazy
-- (in addition to the calls in alpha / beta)


tableau : Decoder Tableau.Tableau
tableau =
    lazy
        (\_ ->
            field "type" string
                |> andThen tblTypeDecoder
        )


decode : String -> ( Result Error Config, Result Error Tableau )
decode s =
    let
        decodeTableau =
            decodeString tableau >> Result.map reRefTableau

        decodeConfig =
            decodeString config
    in
    ( decodeConfig s, decodeTableau s )


reRefTableau : Tableau.Tableau -> Tableau.Tableau
reRefTableau t =
    t
        |> Zipper.zipper
        |> Zipper.zWalkPost reRef
        |> Zipper.zTableau


reRef : Zipper.Zipper -> Zipper.Zipper
reRef z =
    z
        |> Zipper.setRefs (z |> Zipper.zNode |> .references |> List.map .str |> String.join ",")
        |> Zipper.modifyNode
            (\t ->
                case t.ext of
                    Tableau.Closed r1 r2 ->
                        Tableau.Tableau t.node (Tableau.Closed (r1.str |> Zipper.getRef z) (r2.str |> Zipper.getRef z))

                    _ ->
                        t
            )



{- vim: set sw=2 ts=2 sts=2 et : -}
