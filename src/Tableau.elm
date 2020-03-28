module Tableau exposing (Breadcrumbs, Cell, CellWidth, Closed, Finalization(..), Crumb(..), Node, Ref, Row, Table, Tableau(..), Zipper, above, asHeadedTable, asTable, children, defClosed, defNode, defRef, delete, depth, down, extendAlpha, extendBeta, fAlpha, fBeta, fLeaf, findAbove, fixClosedRefs, fixNodeRef, fixRefs, fixedRef, formula, getRef, getReffed, getReffedFormula, indented, indentedFinalization, indentedNode, indentedRef, isAlpha, isBeta, isLeaf, isLeftChild, isPremise, isRightChild, isRoot, left, makeClosed, makeOpenComplete, makeUnfinished, mapNode, maxNum, merge, modify, modifyNode, modifyRef, node, prettify, renumber, renumber2, right, setClosed, setFormula, setPair1, setRef, tl, top, tt, up, width, zFormula, zNode, zTableau, zWalkPost, zipper)

import Formula exposing (Formula, Signed(..))
import Parser
import Result exposing (Result(..))


type alias Ref =
    { str : String
    , up : Maybe Int
    }


defRef =
    { str = "", up = Nothing }


type alias Node =
    { num : Int
    , text : String
    , ref : Ref
    }


defNode =
    { num = 1, text = "", ref = defRef }


type alias Closed =
    ( Ref, Ref )


defClosed =
    (defRef, defRef)


type Finalization
    = Unfinished
    | Closed Closed
    | OpenComplete


type Tableau
    = Leaf Node Finalization
    | Alpha Node Tableau
    | Beta Node Tableau Tableau


node : Tableau -> Node
node t =
    case t of
        Leaf n _ ->
            n

        Alpha n _ ->
            n

        Beta n _ _ ->
            n


formula : Tableau -> Result (List Parser.DeadEnd) (Signed Formula)
formula t =
    (node t).text
        |> Formula.parseSigned


mapNode : (Node -> Node) -> Tableau -> Tableau
mapNode f t =
    case t of
        Leaf n fin ->
            Leaf (f n) fin

        Alpha n ct ->
            Alpha (f n) (mapNode f ct)

        Beta n lt rt ->
            Beta (f n) (mapNode f lt) (mapNode f rt)



--
-- convert to table
--


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
    case t of
        Leaf n _ ->
            ( ( 1, Just ( t, bs ) ), [] )

        Alpha n st ->
            let
                sz =
                    ( t, bs ) |> down

                ( topCell, table ) =
                    asHeadedTable sz

                ( topWidth, topElem ) =
                    topCell
            in
            ( ( topWidth, Just ( t, bs ) ), [ [ topCell ] ] ++ table )

        Beta n lt rt ->
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
            , [ [ ltop, rtop ] ] ++ merge ltable rtable
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



--
-- Zipper
--


type Crumb
    = AlphaCrumb Node
    | BetaLeftCrumb Node Tableau
    | BetaRightCrumb Node Tableau


type alias Breadcrumbs =
    List Crumb


type alias Zipper =
    ( Tableau, Breadcrumbs )


zipper : Tableau -> Zipper
zipper t =
    ( t, [] )


children : Zipper -> List Zipper
children (( t, bs ) as z) =
    case t of
        Leaf _ _ ->
            []

        Alpha _ _ ->
            [ down z ]

        Beta _ _ _ ->
            [ left z, right z ]


down : Zipper -> Zipper
down ( t, bs ) =
    case t of
        Alpha n st ->
            ( st, AlphaCrumb n :: bs )

        _ ->
            ( t, bs )


left : Zipper -> Zipper
left ( t, bs ) =
    case t of
        Beta n lt rt ->
            ( lt, BetaLeftCrumb n rt :: bs )

        _ ->
            ( t, bs )


right : Zipper -> Zipper
right ( t, bs ) =
    case t of
        Beta n lt rt ->
            ( rt, BetaRightCrumb n lt :: bs )

        _ ->
            ( t, bs )


up : Zipper -> Zipper
up ( t, bs ) =
    case bs of
        (AlphaCrumb n) :: bss ->
            ( Alpha n t, bss )

        (BetaLeftCrumb n rt) :: bss ->
            ( Beta n t rt, bss )

        (BetaRightCrumb n lt) :: bss ->
            ( Beta n lt t, bss )

        [] ->
            ( t, bs )


above : Int -> Zipper -> Zipper
above n z =
    case n of
        0 ->
            z

        _ ->
            above (n - 1) (up z)


top : Zipper -> Zipper
top ( t, bs ) =
    case bs of
        [] ->
            ( t, bs )

        _ ->
            top (up ( t, bs ))


modify : (Tableau -> Tableau) -> Zipper -> Zipper
modify f ( t, bs ) =
    ( f t, bs )


modifyNode : (Node -> Node) -> Zipper -> Zipper
modifyNode f =
    modify
        (\t ->
            case t of
                Leaf n fin ->
                    Leaf (f n) fin

                Alpha n st ->
                    Alpha (f n) st

                Beta n lt rt ->
                    Beta (f n) lt rt
        )


zTableau : Zipper -> Tableau
zTableau ( t, bs ) =
    t


zNode : Zipper -> Node
zNode =
    zTableau >> node


zFormula : Zipper -> Result (List Parser.DeadEnd) (Signed Formula)
zFormula =
    zTableau >> formula


zWalkPost : (Zipper -> Zipper) -> Zipper -> Zipper
zWalkPost f (( t, bs ) as z) =
    case t of
        Leaf _ _ ->
            z |> f

        Alpha n st ->
            z |> down |> zWalkPost f |> up |> f

        Beta n lt rt ->
            z |> left |> zWalkPost f |> up |> right |> zWalkPost f |> up |> f



--
-- Actions
--


setFormula : String -> Zipper -> Zipper
setFormula text =
    modifyNode (\n -> { n | text = text })


findAbove : Int -> Zipper -> Maybe Int
findAbove ref ( t, bs ) =
    if (node t).num == ref then
        Just 0

    else
        case bs of
            a :: bbs ->
                Maybe.map ((+) 1) (( t, bs ) |> up |> findAbove ref)

            [] ->
                Nothing


getRef : String -> Zipper -> Ref
getRef ref z =
    { str = ref
    , up =
        ref
            |> String.toInt
            |> Maybe.andThen ((\b a -> findAbove a b) z)
    }


setRef : String -> Zipper -> Zipper
setRef ref z =
    z |> modifyRef (getRef ref z)


getReffed : Ref -> Zipper -> Maybe Zipper
getReffed r z =
    r.up
        |> Maybe.map ((\b a -> above a b) z)


getReffedFormula : Ref -> Zipper -> Maybe (Signed Formula)
getReffedFormula r z =
    z
        |> getReffed r
        |> Maybe.andThen (zFormula >> Result.toMaybe)


modifyRef : Ref -> Zipper -> Zipper
modifyRef ref =
    modifyNode (\n -> { n | ref = ref })


makeClosed : Zipper -> Zipper
makeClosed =
    modify
        (\t ->
            case t of
                Leaf n _ ->
                    Leaf n (Closed defClosed)

                _ ->
                    t
        )


makeUnfinished : Zipper -> Zipper
makeUnfinished =
    modify
        (\t ->
            case t of
                Leaf n _ ->
                    Leaf n Unfinished

                _ ->
                    t
        )


makeOpenComplete : Zipper -> Zipper
makeOpenComplete =
    modify
        (\t ->
            case t of
                Leaf n _ ->
                    Leaf n OpenComplete

                _ ->
                    t
        )


setPair1 : Int -> ( a, a ) -> a -> ( a, a )
setPair1 which p n =
    let
        ( a, b ) =
            p
    in
    case which of
        0 ->
            ( n, b )

        _ ->
            ( a, n )


setClosed : Int -> String -> Zipper -> Zipper
setClosed which ref z =
    z
        |> modify
            (\t ->
                case t of
                    Leaf n (Closed p) as cls ->
                        Leaf n (Closed (setPair1 which p (z |> getRef ref)))

                    _ ->
                        t
            )


extendAlpha : Zipper -> Zipper
extendAlpha =
    modify
        (\t ->
            case t of
                Leaf n fin ->
                    Alpha n
                        (Leaf defNode fin)

                _ ->
                    t
        )


extendBeta : Zipper -> Zipper
extendBeta =
    modify
        (\t ->
            case t of
                Leaf n fin ->
                    Beta n
                        (Leaf defNode fin)
                        (Leaf defNode fin)

                _ ->
                    t
        )


delete : Zipper -> Zipper
delete z =
    if isRoot z then
        resetAndDeleteBelow z
    else
        deleteBelow (up z)


resetAndDeleteBelow : Zipper -> Zipper
resetAndDeleteBelow =
    modify (\t -> Leaf defNode Unfinished)


deleteBelow : Zipper -> Zipper
deleteBelow =
    modify (\t -> Leaf (node t) Unfinished)


renumber : Tableau -> Tableau
renumber t =
    renumber2 t 0
        |> Tuple.first
        |> zipper
        |> fixRefs
        |> top
        |> zTableau


fixRefs : Zipper -> Zipper
fixRefs =
    zWalkPost (fixNodeRef >> fixClosedRefs)


fixedRef : Ref -> Zipper -> Ref
fixedRef ref z =
    case ref.up of
        Nothing ->
            { ref | str = "" }

        -- invalid refs might become valid, better explicitely clean them up...
        Just n ->
            { ref | str = z |> above n |> zNode |> .num |> String.fromInt }


fixNodeRef : Zipper -> Zipper
fixNodeRef z =
    z |> modifyNode (\n -> { n | ref = fixedRef n.ref z })


fixClosedRefs : Zipper -> Zipper
fixClosedRefs z =
    z
        |> modify
            (\t ->
                case t of
                    Leaf n (Closed ( a, b )) ->
                        Leaf n (Closed ( fixedRef a z, fixedRef b z ))

                    _ ->
                        t
            )


renumber2 : Tableau -> Int -> ( Tableau, Int )
renumber2 t num =
    case t of
        Leaf n fin ->
            ( Leaf { n | num = num + 1 } fin, num + 1 )

        Alpha n st ->
            let
                ( nst, num1 ) =
                    renumber2 st (num + 1)
            in
            ( Alpha { n | num = num + 1 } nst, num1 )

        Beta n lt rt ->
            let
                ( nlt, num1 ) =
                    renumber2 lt (num + 1)

                ( nrt, num2 ) =
                    renumber2 rt num1
            in
            ( Beta { n | num = num + 1 } nlt nrt, num2 )


prettify : Tableau -> Tableau
prettify =
    mapNode
        (\n ->
            { n
                | text =
                    case Formula.parseSigned n.text of
                        Ok f ->
                            Formula.strSigned f

                        Err e ->
                            n.text
            }
        )



--
-- tests
--


isRoot (_, bs) =
    List.isEmpty bs


isPremise z =
    case z |> zNode |> .ref |> .up of
        Just 0 ->
            True

        _ ->
            False


isAlpha ( t, _ ) =
    case t of
        Alpha _ _ ->
            True

        _ ->
            False


isBeta ( t, _ ) =
    case t of
        Beta _ _ _ ->
            True

        _ ->
            False


isLeaf ( t, _ ) =
    case t of
        Leaf _ _ ->
            True

        _ ->
            False


isLeftChild ( _, bs ) =
    case bs of
        BetaLeftCrumb _ _ :: _ ->
            True

        _ ->
            False


isRightChild ( _, bs ) =
    case bs of
        BetaRightCrumb _ _ :: _ ->
            True

        _ ->
            False



--
-- debug print funcs
--


indentedRef r =
    "[" ++ r.str ++ "]"


indentedNode ind n =
    let
        parsedFrm =
            if n.text == "" then
                ""

            else
                case Formula.parseSigned n.text of
                    Ok f ->
                        Formula.strSigned f

                    Err e ->
                        Debug.toString e
    in
    String.repeat ind " "
        ++ "("
        ++ String.fromInt n.num
        ++ ")"
        ++ parsedFrm
        ++ " "
        ++ indentedRef n.ref


indentedFinalization mc =
    case mc of
        Unfinished ->
            ""

        Closed ( a, b ) ->
            "*(" ++ a.str ++ "," ++ b.str ++ ")"

        OpenComplete ->
            "OC"


indented ind t =
    case t of
        Leaf n fin ->
            indentedNode ind n ++ " " ++ indentedFinalization fin ++ "\n"

        Alpha n ct ->
            indentedNode ind n ++ "\n" ++ indented (ind + 2) ct

        Beta n lt rt ->
            indentedNode ind n ++ "\n" ++ indented (ind + 2) lt ++ indented (ind + 2) rt


width : Tableau -> Int
width t =
    case t of
        Leaf _ _ ->
            1

        Alpha _ ct ->
            width ct

        Beta _ lt rt ->
            width lt + width rt



--
-- debug funcs
--


depth : Tableau -> Int
depth t =
    case t of
        Leaf _ _ ->
            1

        Alpha _ ct ->
            1 + depth ct

        Beta _ lt rt ->
            1 + depth lt + depth rt


fLeaf text =
    Leaf { defNode | text = text } Unfinished


fAlpha text ct =
    let
        nt =
            mapNode (\n -> { n | num = n.num + 1 }) ct
    in
    Alpha { defNode | text = text } nt


fBeta text lt rt =
    let
        nlt =
            mapNode (\n -> { n | num = n.num + 1 }) lt

        nrt =
            mapNode (\n -> { n | num = n.num + maxNum nlt }) rt
    in
    Beta { defNode | text = text } nlt nrt


maxNum : Tableau -> Int
maxNum t =
    case t of
        Leaf n _ ->
            n.num

        Alpha n ct ->
            max n.num (maxNum ct)

        Beta n lt rt ->
            max n.num (max (maxNum lt) (maxNum rt))


tl =
    fAlpha "T(a&b)"
        (fAlpha "Ta"
            (fAlpha "Tb"
                (fLeaf "Tc")
            )
        )


tt =
    fBeta "T((a&b)|c)"
        (fAlpha "F (a&b)"
            (fLeaf "Ta")
        )
        (fLeaf "Fc")



{- vim: set sw=2 ts=2 sts=2 et : -}
