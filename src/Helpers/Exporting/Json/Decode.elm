module Helpers.Exporting.Json.Decode exposing (decode, tableau)

import Formula
import Json.Decode exposing (..)
import Tableau
import Zipper


{- unfortunately we need to parse the refs
   in a second pass
-}


mkRef : String -> Tableau.Ref
mkRef str =
    { str = str, up = Nothing }


ref =
    map mkRef string


closedRefs =
    map2 (,)
        (index 0 ref)
        (index 1 ref)


node =
    let
        id =
            field "id" int

        value =
            field "value" string

        reference =
            field "reference" ref
    in
    map4
        Tableau.Node
        (field "id" int)
        (field "value" string)
        (field "reference" ref)
        (Formula.parseSigned (field "value" string))


substitution =
    map2 Tableau.Substitution
        (field "what" string)
        (field "forWhat" string)


open =
    map2
        Tableau.Tableau
        (field "node" node)
        (lazy
            (\_ -> Tableau.Open)
        )


closed =
    let
        ( r1, r2 ) =
            field "closed" closedRefs
    in
    map2
        Tableau.Tableau
        (field "node" node)
        (Tableau.Closed r1 r2)


alpha =
    map2
        Tableau.Tableau
        (field "node" node)
        (lazy (\_ -> Tableau.Alpha (field "child" (Json.Decode.lazy (\_ -> tableau)))))


beta =
    map2 Tableau.Tableau
        (field "node" node)
        (lazy (\_ -> Tableau.Beta (field "leftChild" (lazy (\_ -> tableau))) (field "rightChild" (lazy (\_ -> tableau)))))


delta =
    map2
        Tableau.Tableau
        (field "node" node)
        (lazy (\_ -> Tableau.Delta (field "child" (lazy (\_ -> tableau))) (lazy (field "substitution" substitution))))


gamma =
    map2
        Tableau.Tableau
        (field "node" node)
        (lazy (\_ -> Tableau.Gamma (field "child" (lazy (\_ -> tableau))) (lazy (\_ -> field "substitution" substitution))))


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


decode =
    decodeString tableau >> Result.map reRefTableau


reRefTableau : Tableau.Tableau -> Tableau.Tableau
reRefTableau t =
    t
        |> Zipper.zipper
        |> Zipper.zWalkPost reRef
        |> Zipper.zTableau


reRef : Zipper.Zipper -> Zipper.Zipper
reRef z =
    z
        |> Zipper.setRef (z |> Zipper.zNode |> .reference |> .str)
        |> Zipper.modifyNode
            (\t ->
                case t.ext of
                    Tableau.Closed r1 r2 ->
                        Tableau.Tableau t.node (Tableau.Closed (z |> Zipper.getRef r1.str) (z |> Zipper.getRef r2.str))

                    _ ->
                        t
            )



{- vim: set sw=2 ts=2 sts=2 et : -}
