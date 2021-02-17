module Helpers.Exporting.Json.Decode exposing (decode, tableau)

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
        (field "term" string)
        (field "var" string)


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


unaryRule : (Tableau.Tableau -> Tableau.Extension) -> Decoder Tableau.Tableau
unaryRule extType =
    map2
        Tableau.Tableau
        (field "node" node)
        (map extType (field "child" (Json.Decode.lazy (\_ -> tableau))))


unaryRuleWithSubst : (Tableau.Tableau -> Tableau.Substitution -> Tableau.Extension) -> Decoder Tableau.Tableau
unaryRuleWithSubst extType =
    map2
        Tableau.Tableau
        (field "node" node)
        (map2 extType (field "child" (lazy (\_ -> tableau))) (field "substitution" substitution))


alpha : Decoder Tableau.Tableau
alpha =
    unaryRule Tableau.Alpha


beta : Decoder Tableau.Tableau
beta =
    map2 Tableau.Tableau
        (field "node" node)
        (map2 Tableau.Beta (field "leftChild" (lazy (\_ -> tableau))) (field "rightChild" (lazy (\_ -> tableau))))


delta : Decoder Tableau.Tableau
delta =
    unaryRuleWithSubst Tableau.Delta


gamma : Decoder Tableau.Tableau
gamma =
    unaryRuleWithSubst Tableau.Gamma


refl : Decoder Tableau.Tableau
refl =
    unaryRule Tableau.Refl


leibnitz : Decoder Tableau.Tableau
leibnitz =
    unaryRule Tableau.Leibnitz


tblTypeDecoder : String -> Decoder Tableau.Tableau
tblTypeDecoder typ =
    case typ of
        "open" ->
            open

        "closed" ->
            closed

        "alpha" ->
            alpha

        "beta" ->
            beta

        "gamma" ->
            gamma

        "delta" ->
            delta

        "refl" ->
            refl

        "leibnitz" ->
            leibnitz

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


decode : String -> Result Error Tableau.Tableau
decode s =
    let
        fn =
            decodeString tableau >> Result.map reRefTableau
    in
    fn s


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
