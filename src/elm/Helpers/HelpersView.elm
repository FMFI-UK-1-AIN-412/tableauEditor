module HelpersView exposing (..)

import Tableau exposing (..)
import Zipper exposing (..)


type alias CellWidth =
    Int


type alias Cell =
    ( CellWidth, Maybe Zipper )



-- the 'Node' at that point


type alias Row =
    List Cell


type alias Table =
    List Row


asTable : Tableau -> Table
asTable t =
    let
        z =
            zipper t

        ( c, tbl ) =
            asHeadedTable z
    in
        [ [ c ] ] ++ tbl


asHeadedTable : Zipper -> ( Cell, Table )
asHeadedTable ( t, bs ) =
    case t.ext of
        Open ->
            ( ( 1, Just ( t, bs ) ), [] )

        Closed _ _ ->
            ( ( 1, Just ( t, bs ) ), [] )

        Alpha tableau ->
            let
                sz =
                    ( t, bs ) |> down

                ( top, table ) =
                    asHeadedTable sz

                ( topWidth, topElem ) =
                    top
            in
                ( ( topWidth, Just ( t, bs ) ), [ [ top ] ] ++ table )

        Beta ltableau rtableau ->
            let
                lz =
                    ( t, bs ) |> left

                rz =
                    ( t, bs ) |> right

                ( ltop, ltable ) =
                    asHeadedTable lz

                ( ltopWidth, ltopE ) =
                    ltop

                ( rtop, rtable ) =
                    asHeadedTable rz

                ( rtopWidth, rtopE ) =
                    rtop
            in
                ( ( ltopWidth + rtopWidth, Just ( t, bs ) )
                , [ [ ltop, rtop ] ] ++ (merge ltable rtable)
                )



-- grr, no asymetric map2 ;(


merge : List (List a) -> List (List a) -> List (List a)
merge ll rl =
    case ( ll, rl ) of
        ( lh :: lt, rh :: rt ) ->
            (lh ++ rh) :: merge lt rt

        ( [], rh :: rt ) ->
            rh :: merge [] rt

        ( lh :: lt, [] ) ->
            lh :: merge lt []

        ( [], [] ) ->
            []


isBeta : Zipper.Zipper -> Bool
isBeta ( t, _ ) =
    case t.ext of
        Beta tl tr ->
            True

        _ ->
            False


isPremise : Zipper.Zipper -> Bool
isPremise z =
    case z |> zNode |> .reference |> .up of
        Just 0 ->
            True

        _ ->
            False
