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

        _ ->
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

        Alpha _ ->
            z |> down |> zWalkPost f |> up |> f

        Beta tl tr ->
            z |> left |> zWalkPost f |> up |> right |> zWalkPost f |> up |> f

        Gamma _ subst ->
            z |> down |> zWalkPost f |> up |> f

        Delta _ subst ->
            z |> down |> zWalkPost f |> up |> f


fixRefs : Zipper -> Zipper
fixRefs =
    zWalkPost (fixNodeRef >> fixClosedRefs)



-- Na kazdy vrchol zavolame najskor fixNodeRef a potom fixCloseRefs


getFixedRef : Ref -> Zipper -> Ref
getFixedRef ref z =
    case ref.up of
        Nothing ->
            { ref | str = "" }

        Just n ->
            { ref | str = z |> above n |> zNode |> .id |> String.fromInt }


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

        Closed r1 r2 ->
            let
                node =
                    tableau.node

                ext =
                    tableau.ext
            in
            ( Tableau { node | id = num + 1 } ext, num + 1 )


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
            |> Maybe.andThen (\a -> findAbove a z)
    }


getReffed : Ref -> Zipper -> Maybe Zipper
getReffed r z =
    r.up
        |> Maybe.map (\a -> above a z)


setPair : Int -> Ref -> Ref -> Ref -> ( Ref, Ref )
setPair which ref r1 r2 =
    case which of
        0 ->
            ( ref, r2 )

        _ ->
            ( r1, ref )


renumberJustInReferences : (Ref -> Int -> Ref) -> Zipper -> Zipper
renumberJustInReferences f z =
    modifyNode
        (\tableau ->
            renumberJusts tableau f 0
        )
        z


renumberJusts : Tableau -> (Ref -> Int -> Ref) -> Int -> Tableau
renumberJusts tableau f lengthOfPathFromFather =
    case tableau.ext of
        Alpha t ->
            Tableau
                tableau.node
                (Alpha (renumberJusts (renumberJust t f (lengthOfPathFromFather + 1)) f (lengthOfPathFromFather + 1)))

        Beta lt rt ->
            Tableau
                tableau.node
                (Beta
                    (renumberJusts (renumberJust lt f (lengthOfPathFromFather + 1)) f (lengthOfPathFromFather + 1))
                    (renumberJusts (renumberJust rt f (lengthOfPathFromFather + 1)) f (lengthOfPathFromFather + 1))
                )

        Gamma t s ->
            Tableau
                tableau.node
                (Gamma (renumberJusts (renumberJust t f (lengthOfPathFromFather + 1)) f (lengthOfPathFromFather + 1)) s)

        Delta t s ->
            Tableau
                tableau.node
                (Delta (renumberJusts (renumberJust t f (lengthOfPathFromFather + 1)) f (lengthOfPathFromFather + 1)) s)

        Open ->
            tableau

        Closed r1 r2 ->
            Tableau
                tableau.node
                (Closed
                    (f r1 lengthOfPathFromFather)
                    (f r2 lengthOfPathFromFather)
                )


renumberJustInRefWhenDeleting : Ref -> Int -> Ref
renumberJustInRefWhenDeleting ref lengthOfPathFromFather =
    case ref.up of
        Just 0 ->
            ref

        Just x ->
            if (x - 1) >= lengthOfPathFromFather then
                Ref ref.str (Just (x - 1))

            else
                ref

        Nothing ->
            ref


renumberJustInRefWhenExpanding : Ref -> Int -> Ref
renumberJustInRefWhenExpanding ref lengthOfPathFromFather =
    case ref.up of
        Just 0 ->
            ref

        Just x ->
            if x + 1 >= lengthOfPathFromFather then
                Ref ref.str (Just (x + 1))

            else
                ref

        Nothing ->
            ref


renumberJust : Tableau -> (Ref -> Int -> Ref) -> Int -> Tableau
renumberJust t f lengthOfPathFromFather =
    case t.node.reference.up of
        Just 0 ->
            t

        Just x ->
            let
                oldReference =
                    t.node.reference

                oldNode =
                    t.node

                newNode =
                    { oldNode | reference = f oldReference lengthOfPathFromFather }

                newTableau =
                    { t | node = newNode }
            in
            newTableau

        Nothing ->
            t


closeControls : Node -> Node
closeControls oldNode =
    { oldNode | gui = { controlsShown = False } }



--Actions


setFormula : String -> Zipper -> Zipper
setFormula text =
    modifyNode
        (\tableau ->
            let
                oldNode =
                    tableau.node
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
                        Tableau (closeControls tableau.node) (Alpha (Tableau defNode Open))

                    Alpha t ->
                        Tableau (closeControls tableau.node) (Alpha (Tableau defNode (Alpha t)))

                    Beta lt rt ->
                        Tableau (closeControls tableau.node) (Alpha (Tableau defNode (Beta lt rt)))

                    Gamma t s ->
                        Tableau (closeControls tableau.node) (Alpha (Tableau defNode (Gamma t s)))

                    Delta t s ->
                        Tableau (closeControls tableau.node) (Alpha (Tableau defNode (Delta t s)))

                    Closed r1 r2 ->
                        Tableau (closeControls tableau.node) (Alpha (Tableau defNode (Closed r1 r2)))
            )


extendBeta : Zipper -> Zipper
extendBeta z =
    z
        |> modifyNode
            (\tableau ->
                case tableau.ext of
                    Open ->
                        Tableau (closeControls tableau.node) (Beta (Tableau defNode Open) (Tableau defNode Open))

                    Alpha t ->
                        Tableau (closeControls tableau.node) (Beta (Tableau defNode (Alpha t)) (Tableau defNode Open))

                    Beta lt rt ->
                        Tableau (closeControls tableau.node) (Beta (Tableau defNode (Beta lt rt)) (Tableau defNode Open))

                    Gamma t s ->
                        Tableau (closeControls tableau.node) (Beta (Tableau defNode (Gamma t s)) (Tableau defNode Open))

                    Delta t s ->
                        Tableau (closeControls tableau.node) (Beta (Tableau defNode (Delta t s)) (Tableau defNode Open))

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
                        Tableau (closeControls tableau.node) (Gamma (Tableau defNode Open) defSubstitution)

                    Alpha t ->
                        Tableau (closeControls tableau.node) (Gamma (Tableau defNode (Alpha t)) defSubstitution)

                    Beta lt rt ->
                        Tableau (closeControls tableau.node) (Gamma (Tableau defNode (Beta lt rt)) defSubstitution)

                    Gamma t s ->
                        Tableau (closeControls tableau.node) (Gamma (Tableau defNode (Gamma t s)) defSubstitution)

                    Delta t s ->
                        Tableau (closeControls tableau.node) (Gamma (Tableau defNode (Delta t s)) defSubstitution)

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
                        Tableau (closeControls tableau.node) (Delta (Tableau defNode Open) defSubstitution)

                    Alpha t ->
                        Tableau (closeControls tableau.node) (Delta (Tableau defNode (Alpha t)) defSubstitution)

                    Beta lt rt ->
                        Tableau (closeControls tableau.node) (Delta (Tableau defNode (Beta lt rt)) defSubstitution)

                    Gamma t s ->
                        Tableau (closeControls tableau.node) (Delta (Tableau defNode (Gamma t s)) defSubstitution)

                    Delta t s ->
                        Tableau (closeControls tableau.node) (Delta (Tableau defNode (Delta t s)) defSubstitution)

                    _ ->
                        tableau
            )


delete : Zipper -> Zipper
delete z =
    modifyNode
        (\tableau ->
            Tableau tableau.node Open
        )
        z


deleteMe : Zipper -> Zipper
deleteMe (( t, fatherbs ) as zip) =
    if (zip |> up) == zip then
        modifyNode
            (\tableau ->
                case tableau.ext of
                    Open ->
                        Tableau defNode Open

                    Closed r1 r2 ->
                        Tableau defNode Open

                    Alpha st ->
                        st

                    Beta lt rt ->
                        -- mozem zmazat iba ked jedna z biet sa moze stat alfou
                        if lt.node.value == "" then
                            rt

                        else if rt.node.value == "" then
                            lt

                        else
                            tableau

                    Gamma st s ->
                        st

                    Delta st s ->
                        st
            )
            zip

    else
        case fatherbs of
            (BetaLeftCrumb fatherNode tr) :: bss ->
                modifyNode
                    (\tableau ->
                        case tableau.ext of
                            Beta lt rt ->
                                if lt.node.value == "" then
                                    Tableau tableau.node (Alpha rt)

                                else
                                    tableau

                            _ ->
                                tableau
                    )
                    (zip |> up)

            (BetaRightCrumb fatherNode tl) :: bss ->
                modifyNode
                    (\tableau ->
                        case tableau.ext of
                            Beta lt rt ->
                                if rt.node.value == "" then
                                    Tableau tableau.node (Alpha lt)

                                else
                                    tableau

                            _ ->
                                tableau
                    )
                    (zip |> up)

            _ ->
                modifyNode
                    (\tableau ->
                        -- som na pozicii otca mazaneho
                        case tableau.ext of
                            Open ->
                                tableau

                            Closed r1 r2 ->
                                Tableau tableau.node Open

                            Alpha st ->
                                Tableau tableau.node st.ext

                            Gamma st s ->
                                Tableau tableau.node st.ext

                            Delta st s ->
                                Tableau tableau.node st.ext

                            _ ->
                                tableau
                    )
                    (zip |> up)


makeClosed : Zipper -> Zipper
makeClosed z =
    modifyNode
        (\tableau ->
            case tableau.ext of
                _ ->
                    Tableau tableau.node (Closed defRef defRef)
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
                    Tableau tableau.node (Gamma t { subs | var = newVariable })

                Delta t subs ->
                    Tableau tableau.node (Delta t { subs | var = newVariable })

                _ ->
                    tableau
        )
        (z |> up)


changeTerm : String -> Zipper -> Zipper
changeTerm newTerm z =
    modifyNode
        (\tableau ->
            case tableau.ext of
                Gamma t subs ->
                    Tableau tableau.node (Gamma t { subs | term = newTerm })

                Delta t subs ->
                    Tableau tableau.node (Delta t { subs | term = newTerm })

                _ ->
                    tableau
        )
        (z |> up)


switchBetas : Zipper -> Zipper
switchBetas z =
    modifyNode
        (\tableau ->
            case tableau.ext of
                Beta lt rt ->
                    Tableau tableau.node (Beta rt lt)

                _ ->
                    tableau
        )
        z


changeButtonAppearance : Zipper -> Zipper
changeButtonAppearance z =
    modifyNode
        (\tableau ->
            let
                oldNode =
                    tableau.node

                oldGUI =
                    tableau.node.gui

                newGUI =
                    { oldGUI | controlsShown = not oldGUI.controlsShown }

                newNode =
                    { oldNode | gui = newGUI }
            in
            { tableau | node = newNode }
        )
        z


changeToAlpha : Zipper -> Zipper
changeToAlpha z =
    if (z |> up) == z then
        z

    else
        modifyNode
            (\tableau ->
                -- pozor na koren
                case tableau.ext of
                    Beta lt rt ->
                        if lt.node.value == "" then
                            Tableau tableau.node (Alpha rt)

                        else if rt.node.value == "" then
                            Tableau tableau.node (Alpha lt)

                        else
                            Tableau tableau.node (Beta lt rt)

                    Gamma t s ->
                        Tableau tableau.node (Alpha t)

                    Delta t s ->
                        Tableau tableau.node (Alpha t)

                    _ ->
                        tableau
            )
            (z |> up)


changeToBeta : Zipper -> Zipper
changeToBeta z =
    if (z |> up) == z then
        z

    else
        modifyNode
            (\tableau ->
                -- pozor na koren
                case tableau.ext of
                    Alpha t ->
                        Tableau tableau.node (Beta t (Tableau defNode Open))

                    Gamma t s ->
                        Tableau tableau.node (Beta t (Tableau defNode Open))

                    Delta t s ->
                        Tableau tableau.node (Beta t (Tableau defNode Open))

                    _ ->
                        tableau
            )
            (z |> up)


changeToGamma : Zipper -> Zipper
changeToGamma z =
    if (z |> up) == z then
        z

    else
        modifyNode
            (\tableau ->
                -- pozor na koren
                case tableau.ext of
                    Alpha t ->
                        Tableau tableau.node (Gamma t defSubstitution)

                    Beta lt rt ->
                        if lt.node.value == "" then
                            Tableau tableau.node (Gamma rt defSubstitution)

                        else if rt.node.value == "" then
                            Tableau tableau.node (Gamma lt defSubstitution)

                        else
                            Tableau tableau.node (Beta lt rt)

                    Delta t s ->
                        Tableau tableau.node (Gamma t s)

                    _ ->
                        tableau
            )
            (z |> up)


changeToDelta : Zipper -> Zipper
changeToDelta z =
    if (z |> up) == z then
        z

    else
        modifyNode
            (\tableau ->
                -- pozor na koren
                case tableau.ext of
                    Alpha t ->
                        Tableau tableau.node (Delta t defSubstitution)

                    Beta lt rt ->
                        if lt.node.value == "" then
                            Tableau tableau.node (Delta rt defSubstitution)

                        else if rt.node.value == "" then
                            Tableau tableau.node (Delta lt defSubstitution)

                        else
                            Tableau tableau.node (Beta lt rt)

                    Gamma t s ->
                        Tableau tableau.node (Delta t s)

                    _ ->
                        tableau
            )
            (z |> up)


prettify : Tableau -> Tableau
prettify t =
    let
        z =
            zipper t

        prettifyNode : Tableau.Node -> Tableau.Node
        prettifyNode n =
            let
                newValue =
                    case Formula.parseSigned n.value of
                        Ok f ->
                            Formula.strSigned f

                        Err _ ->
                            n.value
            in
            { n | value = newValue }
    in
    z
        |> modifyNode
            (\tableau ->
                case tableau.ext of
                    Alpha tbl ->
                        Tableau (tableau.node |> prettifyNode) (Alpha (prettify tbl))

                    Beta tl tr ->
                        Tableau (tableau.node |> prettifyNode) (Beta (prettify tl) (prettify tr))

                    Gamma tbl subs ->
                        Tableau (tableau.node |> prettifyNode) (Gamma (prettify tbl) subs)

                    Delta tbl subs ->
                        Tableau (tableau.node |> prettifyNode) (Delta (prettify tbl) subs)

                    Open ->
                        Tableau (tableau.node |> prettifyNode) Open

                    Closed r1 r2 ->
                        Tableau (tableau.node |> prettifyNode) (Closed r1 r2)
            )
        |> zTableau
