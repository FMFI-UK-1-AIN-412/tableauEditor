module Zipper exposing (..)

import Debug exposing (log)
import Formula
import Tableau exposing (..)


--crumb hovori o tom, kto je podo mnou


type Crumb
    = AlphaCrumb Node
    | BetaLeftCrumb Node Tableau
    | BetaRightCrumb Node Tableau
    | GammaCrumb Node Tableau.Substitution
    | DeltaCrumb Node Tableau.Substitution


type alias BreadCrumbs =
    List Crumb


type alias Zipper =
    ( Tableau, BreadCrumbs )


zipper : Tableau -> Zipper
zipper t =
    ( t, [] )


children : Zipper -> List Zipper
children z =
    let
        ( t, bs ) =
            z
    in
    case t.ext of
        Open ->
            []

        Closed _ _ ->
            []

        Alpha _ ->
            [ down z ]

        Beta _ _ ->
            [ left z, right z ]

        Gamma _ _ ->
            [ down z ]

        Delta _ _ ->
            [ down z ]


down : Zipper -> Zipper
down ( t, bs ) =
    case t.ext of
        Alpha subt ->
            ( subt, AlphaCrumb t.node :: bs )

        Gamma subtableau substitution ->
            ( subtableau, GammaCrumb t.node substitution :: bs )

        Delta subtableau substitution ->
            ( subtableau, DeltaCrumb t.node substitution :: bs )

        _ ->
            ( t, bs )


right : Zipper -> Zipper
right ( t, bs ) =
    case t.ext of
        Beta tl tr ->
            ( tr, BetaRightCrumb t.node tl :: bs )

        _ ->
            ( t, bs )



--vracia zipper pre mojho laveho syna


left : Zipper -> Zipper
left ( t, bs ) =
    case t.ext of
        Beta tl tr ->
            ( tl, BetaLeftCrumb t.node tr :: bs )

        _ ->
            ( t, bs )


up : Zipper -> Zipper
up ( t, bs ) =
    case bs of
        (AlphaCrumb n) :: bss ->
            ( Tableau n (Alpha t), bss )

        (BetaLeftCrumb n tr) :: bss ->
            ( Tableau n (Beta t tr), bss )

        (BetaRightCrumb n tl) :: bss ->
            ( Tableau n (Beta tl t), bss )

        (GammaCrumb n subst) :: bss ->
            ( Tableau n (Gamma t subst), bss )

        (DeltaCrumb n subst) :: bss ->
            ( Tableau n (Delta t subst), bss )

        [] ->
            ( t, bs )


top : Zipper -> Zipper
top ( t, bs ) =
    case bs of
        [] ->
            ( t, bs )

        _ ->
            top (up ( t, bs ))


above : Int -> Zipper -> Zipper
above n z =
    case n of
        0 ->
            z

        n ->
            above (n - 1) (up z)



--helpers


modifyNode : (Tableau -> Tableau) -> Zipper -> Zipper
modifyNode f ( tableau, bs ) =
    ( f tableau, bs )


zTableau : Zipper -> Tableau
zTableau ( t, bs ) =
    t


zNode : Zipper -> Node
zNode z =
    (zTableau z).node


zSubstitution : Zipper -> Maybe Tableau.Substitution
zSubstitution (( t, bs ) as z) =
    case t.ext of
        Gamma node subs ->
            Just subs

        Delta node subs ->
            Just subs

        _ ->
            Nothing



-- "Aplikuje" funkciu f na kazdy vrchol v zipperi


zWalkPost : (Zipper -> Zipper) -> Zipper -> Zipper
zWalkPost f (( t, bs ) as z) =
    case t.ext of
        Open ->
            f z

        Closed _ _ ->
            f z

        Alpha t ->
            z |> down |> zWalkPost f |> up |> f

        Beta tl tr ->
            z |> left |> zWalkPost f |> up |> right |> zWalkPost f |> up |> f

        Gamma t subst ->
            z |> down |> zWalkPost f |> up |> f

        Delta t subst ->
            z |> down |> zWalkPost f |> up |> f


fixRefs : Zipper -> Zipper
fixRefs =
    zWalkPost (fixNodeRef >> fixClosedRefs)



-- Na kazdy vrchol zavolame najskor fixNodeRef a potom fixCloseRefs


getFixedRef : Ref -> Zipper -> Ref
getFixedRef ({ str, up } as ref) z =
    case up of
        Nothing ->
            { ref | str = "" }

        Just n ->
            { ref | str = z |> above n |> zNode |> .id |> toString }


fixNodeRef : Zipper -> Zipper
fixNodeRef z =
    modifyNode
        (\t ->
            let
                nodetmp =
                    t.node
            in
            { t | node = { nodetmp | reference = getFixedRef nodetmp.reference z } }
        )
        z


fixClosedRefs : Zipper -> Zipper
fixClosedRefs z =
    z
        |> modifyNode
            (\t ->
                let
                    ext =
                        t.ext

                    node =
                        t.node
                in
                case ext of
                    Closed ref1 ref2 ->
                        Tableau node (Closed (getFixedRef ref1 z) (getFixedRef ref2 z))

                    _ ->
                        t
            )


renumber : Tableau -> Tableau
renumber tableau =
    renumber2 tableau 0
        |> Tuple.first
        |> zipper
        |> fixRefs
        |> top
        |> zTableau


renumber2 : Tableau -> Int -> ( Tableau, Int )
renumber2 tableau num =
    case tableau.ext of
        Open ->
            let
                node =
                    tableau.node

                ext =
                    tableau.ext
            in
            ( Tableau { node | id = num + 1 } ext, num + 1 )

        Alpha t ->
            let
                ( new_tableau, num1 ) =
                    renumber2 t (num + 1)

                node =
                    tableau.node
            in
            ( Tableau { node | id = num + 1 } (Alpha new_tableau), num1 )

        Beta lt rt ->
            let
                ( new_left, num1 ) =
                    renumber2 lt (num + 1)

                ( new_right, num2 ) =
                    renumber2 rt num1

                node =
                    tableau.node
            in
            ( Tableau { node | id = num + 1 } (Beta new_left new_right), num2 )

        Gamma t subst ->
            let
                ( new_tableau, num1 ) =
                    renumber2 t (num + 1)

                node =
                    tableau.node
            in
            ( Tableau { node | id = num + 1 } (Gamma new_tableau subst), num1 )

        Delta t subst ->
            let
                ( new_tableau, num1 ) =
                    renumber2 t (num + 1)

                node =
                    tableau.node
            in
            ( Tableau { node | id = num + 1 } (Delta new_tableau subst), num1 )

        _ ->
            ( tableau, num )


modifyRef : Ref -> Zipper -> Zipper
modifyRef ref z =
    modifyNode
        (\tableau ->
            let
                nodetmp =
                    tableau.node
            in
            { tableau | node = { nodetmp | reference = ref } }
        )
        z


findAbove : Int -> Zipper -> Maybe Int
findAbove ref ( tableau, bs ) =
    let
        node =
            tableau.node
    in
    if node.id == ref then
        Just 0
    else
        case bs of
            a :: bbs ->
                Maybe.map ((+) 1) (( tableau, bs ) |> up |> findAbove ref)

            [] ->
                Nothing


getRef : String -> Zipper -> Ref
getRef ref z =
    { str = ref
    , up =
        ref
            |> String.toInt
            |> Result.toMaybe
            |> Maybe.andThen (flip findAbove z)
    }


getReffed : Ref -> Zipper -> Maybe Zipper
getReffed r z =
    r.up
        |> Maybe.map (flip above z)


setPair : Int -> Ref -> Ref -> Ref -> ( Ref, Ref )
setPair which ref r1 r2 =
    case which of
        0 ->
            ( ref, r2 )

        _ ->
            ( r1, ref )



--Actions


setFormula : String -> Zipper -> Zipper
setFormula text =
    modifyNode
        (\tableau ->
            let
                oldNode =
                    tableau.node

                -- substitute here
            in
            { tableau | node = { oldNode | value = text, formula = Formula.parseSigned text } }
        )


setRef : String -> Zipper -> Zipper
setRef new z =
    z |> modifyRef (getRef new z)


extendAlpha : Zipper -> Zipper
extendAlpha z =
    z
        |> modifyNode
            (\tableau ->
                case tableau.ext of
                    Open ->
                        Tableau tableau.node (Alpha (Tableau defNode Open))

                    _ ->
                        --tuto dopisat v pripade extendovania nie len pod leafs
                        tableau
            )


extendBeta : Zipper -> Zipper
extendBeta z =
    z
        |> modifyNode
            (\tableau ->
                case tableau.ext of
                    Open ->
                        Tableau tableau.node (Beta (Tableau defNode Open) (Tableau defNode Open))

                    _ ->
                        tableau
            )


extendGamma : Zipper -> Zipper
extendGamma z =
    z
        |> modifyNode
            (\tableau ->
                case tableau.ext of
                    Open ->
                        Tableau tableau.node (Gamma (Tableau defNode Open) defSubstitution)

                    _ ->
                        tableau
            )


extendDelta : Zipper -> Zipper
extendDelta z =
    z
        |> modifyNode
            (\tableau ->
                case tableau.ext of
                    Open ->
                        Tableau tableau.node (Delta (Tableau defNode Open) defSubstitution)

                    _ ->
                        tableau
            )


delete : Zipper -> Zipper
delete z =
    modifyNode (\tableau -> Tableau defNode Open) z


makeClosed : Zipper -> Zipper
makeClosed z =
    modifyNode
        (\tableau ->
            case tableau.ext of
                Open ->
                    Tableau tableau.node (Closed defRef defRef)

                _ ->
                    tableau
        )
        z


setClosed : Int -> String -> Zipper -> Zipper
setClosed which newRefStr z =
    modifyNode
        (\tableau ->
            case tableau.ext of
                Closed r1 r2 ->
                    let
                        newRef =
                            setPair which (z |> getRef newRefStr) r1 r2
                    in
                    Tableau tableau.node (Closed (Tuple.first newRef) (Tuple.second newRef))

                _ ->
                    tableau
        )
        z


makeOpen : Zipper -> Zipper
makeOpen z =
    modifyNode
        (\tableau ->
            case tableau.ext of
                Closed _ _ ->
                    Tableau tableau.node Open

                _ ->
                    tableau
        )
        z


changeVariable : String -> Zipper -> Zipper
changeVariable newVariable z =
    modifyNode
        (\tableau ->
            case tableau.ext of
                Gamma t subs ->
                    Tableau tableau.node (Gamma t { subs | forWhat = newVariable })

                Delta t subs ->
                    Tableau tableau.node (Delta t { subs | forWhat = newVariable })

                _ ->
                    tableau
        )
        z


changeTerm : String -> Zipper -> Zipper
changeTerm newTerm z =
    modifyNode
        (\tableau ->
            case tableau.ext of
                Gamma t subs ->
                    Tableau tableau.node (Gamma t { subs | what = newTerm })

                Delta t subs ->
                    Tableau tableau.node (Delta t { subs | what = newTerm })

                _ ->
                    tableau
        )
        z
