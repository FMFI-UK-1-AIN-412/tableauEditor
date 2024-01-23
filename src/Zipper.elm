module Zipper exposing (..)

import Formula exposing (Formula)
import Formula.Parser
import Formula.Signed exposing (Signed)
import Tableau exposing (..)
import LogicContext exposing (LogicContext)



--crumb hovori o tom, kto je podo mnou


type Crumb
    = UnaryCrumb UnaryExtType Node
    | UnaryCrumbWithSubst UnaryWithSubstExtType Node Tableau.Substitution
    | BinaryLeftCrumb BinaryExtType Node Tableau
    | BinaryRightCrumb BinaryExtType Node Tableau


type alias BreadCrumbs =
    List Crumb

type alias Zipper =
    { tableau : Tableau
    , breadcrumbs : BreadCrumbs
    , logicContext : Result String LogicContext
    }


zipper : Tableau -> Zipper
zipper t =
    Zipper t [] (Err "not-loaded")

zipperOld : Tableau -> BreadCrumbs -> Zipper
zipperOld t bs =
    Zipper t bs (Err "not-loaded")


children : Zipper -> List Zipper
children z =
    case z.tableau.ext of
        Unary _ _ ->
            [ down z ]

        UnaryWithSubst _ _ _ ->
            [ down z ]

        Binary _ _ _ ->
            [ left z, right z ]

        _ ->
            []


down : Zipper -> Zipper
down z =
    case z.tableau.ext of
        Unary extType subT ->
            { z
                | tableau = subT
                , breadcrumbs = UnaryCrumb extType z.tableau.node :: z.breadcrumbs
            }

        UnaryWithSubst extType subT subst ->
            { z
                | tableau = subT
                , breadcrumbs = UnaryCrumbWithSubst extType z.tableau.node subst :: z.breadcrumbs
            }

        _ ->
            z


right : Zipper -> Zipper
right z =
    case z.tableau.ext of
        Binary extType lt rt ->
            { z
                | tableau = rt
                , breadcrumbs = BinaryRightCrumb extType z.tableau.node lt :: z.breadcrumbs
            }

        _ ->
            z



--vracia zipper pre mojho laveho syna


left : Zipper -> Zipper
left z =
    case z.tableau.ext of
        Binary extType lt rt ->
            { z
                | tableau = lt
                , breadcrumbs = BinaryLeftCrumb extType z.tableau.node rt :: z.breadcrumbs
            }

        _ ->
            z


up : Zipper -> Zipper
up z =
    case z.breadcrumbs of
        (UnaryCrumb extType n) :: bss ->
            { z
                | tableau = Tableau n (Unary extType z.tableau)
                , breadcrumbs = bss
            }

        (UnaryCrumbWithSubst extType n subst) :: bss ->
            { z
                | tableau = Tableau n (UnaryWithSubst extType z.tableau subst)
                , breadcrumbs = bss
            }

        (BinaryLeftCrumb extType n rt) :: bss ->
            { z
                | tableau = Tableau n (Binary extType z.tableau rt)
                , breadcrumbs = bss
            }

        (BinaryRightCrumb extType n lt) :: bss ->
            { z
                | tableau = Tableau n (Binary extType lt z.tableau)
                , breadcrumbs = bss
            }

        [] ->
            z


top : Zipper -> Zipper
top z =
    case z.breadcrumbs of
        [] ->
            z

        _ ->
            top (up z)


above : Int -> Zipper -> Zipper
above n z =
    case n of
        0 ->
            z

        _ ->
            above (n - 1) (up z)



--helpers


modifyNode : (Tableau -> Tableau) -> Zipper -> Zipper
modifyNode f z =
    { z
        | tableau = f z.tableau
        , breadcrumbs = z.breadcrumbs
    }


zTableau : Zipper -> Tableau
zTableau z =
    z.tableau


zNode : Zipper -> Node
zNode z =
    (zTableau z).node


zSubstitution : Zipper -> Maybe Tableau.Substitution
zSubstitution z =
    case z.tableau.ext of
        UnaryWithSubst _ _ subst ->
            Just subst

        _ ->
            Nothing


hasReference : Zipper -> Bool
hasReference z =
    List.isEmpty (zNode z).references && ((zNode z).value /= "")


isAssumption : Zipper -> Bool
isAssumption z =
    case up z |> zTableau |> .ext of
        Unary Assumption _ ->
            True

        _ ->
            up z == z


assumptions : Zipper -> List (Signed Formula)
assumptions z =
    (Maybe.map2 (\_ y -> y)
        (if z |> isAssumption then
            Just ()

         else
            Nothing
        )
        (z
            |> zNode
            |> .formula
            |> Result.toMaybe
        )
        |> Maybe.map List.singleton
        |> Maybe.withDefault []
    )
        ++ List.concatMap assumptions (children z)



-- "Aplikuje" funkciu f na kazdy vrchol v zipperi


zWalkPost : (Zipper -> Zipper) -> Zipper -> Zipper
zWalkPost f z =
    case z.tableau.ext of
        Open ->
            f z

        Closed _ _ ->
            f z

        OpenComplete ->
            f z

        Binary _ _ _ ->
            z |> left |> zWalkPost f |> up |> right |> zWalkPost f |> up |> f

        _ ->
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
        (\({ node } as tableau) ->
            { tableau | node = { node | references = List.map (\ref -> getFixedRef ref z) node.references } }
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
    let
        node =
            tableau.node

        ext =
            tableau.ext

        renumberUnary extWithType subT =
            let
                ( new_tableau, num1 ) =
                    renumber2 subT (num + 1)
            in
            ( Tableau { node | id = num + 1 } (extWithType new_tableau), num1 )

        renumberBinary extWithType lt rt =
            let
                ( new_left, num1 ) =
                    renumber2 lt (num + 1)

                ( new_right, num2 ) =
                    renumber2 rt num1
            in
            ( Tableau { node | id = num + 1 } (extWithType new_left new_right), num2 )
    in
    case tableau.ext of
        Open ->
            ( Tableau { node | id = num + 1 } ext, num + 1 )

        Closed _ _ ->
            ( Tableau { node | id = num + 1 } ext, num + 1 )

        OpenComplete ->
            ( Tableau { node | id = num + 1 } ext, num + 1 )

        Unary extType subT ->
            renumberUnary (Unary extType) subT

        UnaryWithSubst extType subT subst ->
            renumberUnary (\t -> UnaryWithSubst extType t subst) subT

        Binary extType lt rt ->
            renumberBinary (Binary extType) lt rt


modifyRef : List Ref -> Zipper -> Zipper
modifyRef refs z =
    modifyNode
        (\({ node } as tableau) ->
            { tableau | node = { node | references = refs } }
        )
        z


findAbove : Int -> Zipper -> Maybe Int
findAbove ref z =
    let
        node =
            z.tableau.node
    in
    if node.id == ref then
        Just 0

    else
        case z.breadcrumbs of
            a :: bbs ->
                Maybe.map ((+) 1) (z |> up |> findAbove ref)

            [] ->
                Nothing


getRef : Zipper -> String -> Ref
getRef z ref =
    { str = ref
    , up =
        ref
            |> String.trim
            |> String.toInt
            |> Maybe.andThen (\a -> findAbove a z)
    }


getReffed : Ref -> Zipper -> Maybe Zipper
getReffed r z =
    r.up
        |> Maybe.map (\a -> above a z)


zFirstRef : Zipper -> Ref
zFirstRef z =
    case z |> zNode |> .references of
        x :: xs ->
            x

        _ ->
            Tableau.defRef


zSecondRef : Zipper -> Ref
zSecondRef z =
    case z |> zNode |> .references of
        x0 :: x1 :: xs ->
            x1

        _ ->
            Tableau.defRef


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
    let
        renumberJustsUnary extWithType subT =
            Tableau
                tableau.node
                (extWithType (renumberJusts (renumberJust subT f (lengthOfPathFromFather + 1)) f (lengthOfPathFromFather + 1)))

        renumberJustsBinary extWithType lt rt =
            Tableau
                tableau.node
                (extWithType
                    (renumberJusts (renumberJust lt f (lengthOfPathFromFather + 1)) f (lengthOfPathFromFather + 1))
                    (renumberJusts (renumberJust rt f (lengthOfPathFromFather + 1)) f (lengthOfPathFromFather + 1))
                )
    in
    case tableau.ext of
        Unary extType subT ->
            renumberJustsUnary (Unary extType) subT

        UnaryWithSubst extType subT subst ->
            renumberJustsUnary (\t -> UnaryWithSubst extType t subst) subT

        Binary extType lt rt ->
            renumberJustsBinary (Binary extType) lt rt

        Open ->
            tableau

        OpenComplete ->
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
    let
        func ref =
            case ref.up of
                Just 0 ->
                    ref

                Just x ->
                    f ref lengthOfPathFromFather

                Nothing ->
                    ref
    in
    let
        oldReferences =
            t.node.references

        oldNode =
            t.node

        newNode =
            { oldNode | references = List.map func oldReferences }

        newTableau =
            { t | node = newNode }
    in
    newTableau


closeControls : Node -> Node
closeControls oldNode =
    { oldNode | gui = { controlsShown = False } }



--Actions


setFormula : String -> Zipper -> Zipper
setFormula text =
    modifyNode
        (\({ node } as tableau) ->
            { tableau | node = { node | value = text, formula = Formula.Parser.parseSigned text } }
        )


setRefs : String -> Zipper -> Zipper
setRefs new z =
    z |> modifyRef (List.map (getRef z) (Tableau.strRefsToList new))


setSubstitution : String -> Zipper -> Zipper
setSubstitution text z =
    modifyNode
        (\tableau ->
            case tableau.ext of
                UnaryWithSubst extType t subst ->
                    Tableau tableau.node (UnaryWithSubst extType t { subst | str = text, parsedSubst = Formula.Parser.parseSubstitution text })

                _ ->
                    tableau
        )
        (z |> up)


extendWithRule : (Tableau -> Extension) -> Zipper -> Zipper
extendWithRule extWithType z =
    z
        |> modifyNode
            (\tableau ->
                Tableau (closeControls tableau.node) (extWithType (Tableau defNode tableau.ext))
            )


extendUnary : Tableau.UnaryExtType -> Zipper -> Zipper
extendUnary extType z =
    extendWithRule (Unary extType) z


extendUnaryWithSubst : Tableau.UnaryWithSubstExtType -> Zipper -> Zipper
extendUnaryWithSubst extType z =
    extendWithRule (\t -> UnaryWithSubst extType t defSubstitution) z


extendBinary : Tableau.BinaryExtType -> Zipper -> Zipper
extendBinary extType z =
    extendWithRule (\t -> Binary extType t (Tableau defNode Open)) z


delete : Zipper -> Zipper
delete z =
    modifyNode
        (\tableau ->
            Tableau tableau.node Open
        )
        z


deleteMe : Zipper -> Zipper
deleteMe zip =
    if (zip |> up) == zip then
        let
            tableauToKeep currentT leftT rightT =
                if leftT.node.value == "" then
                    rightT

                else if rightT.node.value == "" then
                    leftT

                else
                    currentT
        in
        modifyNode
            (\tableau ->
                case tableau.ext of
                    Open ->
                        Tableau defNode Open

                    Closed _ _ ->
                        Tableau defNode Open

                    OpenComplete ->
                        Tableau defNode Open

                    Binary _ lt rt ->
                        tableauToKeep tableau lt rt

                    _ ->
                        Tableau.leftSubtree tableau
            )
            zip

    else
        let
            tryToKeepRightSubT currentT leftT rightT =
                if leftT.node.value == "" then
                    Tableau currentT.node (Unary Alpha rightT)

                else
                    currentT

            tryToKeepLeftSubT currentT leftT rightT =
                if rightT.node.value == "" then
                    Tableau currentT.node (Unary Alpha leftT)

                else
                    currentT

            chooseSubTableau childToKeep =
                modifyNode
                    (\tableau ->
                        childToKeep tableau (Tableau.leftSubtree tableau) (Tableau.rightSubtree tableau)
                    )
                    (zip |> up)
        in
        case zip.breadcrumbs of
            (BinaryLeftCrumb _ farherNode rt) :: bss ->
                chooseSubTableau tryToKeepRightSubT

            (BinaryRightCrumb _ farherNode lt) :: bss ->
                chooseSubTableau tryToKeepLeftSubT

            _ ->
                modifyNode
                    (\tableau ->
                        -- som na pozicii otca mazaneho
                        case tableau.ext of
                            Open ->
                                tableau

                            Closed _ _ ->
                                Tableau tableau.node Open

                            OpenComplete ->
                                Tableau tableau.node Open

                            Binary _ _ _ ->
                                tableau

                            _ ->
                                Tableau tableau.node (Tableau.leftSubtree tableau).ext
                    )
                    (zip |> up)


makeClosed : Zipper -> Zipper
makeClosed z =
    modifyNode
        (\tableau ->
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
                            setPair which (newRefStr |> getRef z) r1 r2
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
            let
                origNode =
                    tableau.node

                origGui =
                    origNode.gui

                openTableauWithControlsShown =
                    Tableau
                        { origNode | gui = { origGui | controlsShown = True } }
                        Open
            in
            case tableau.ext of
                Closed _ _ ->
                    openTableauWithControlsShown

                OpenComplete ->
                    openTableauWithControlsShown

                _ ->
                    tableau
        )
        z


makeOpenComplete : Zipper -> Zipper
makeOpenComplete z =
    modifyNode
        (\tableau ->
            Tableau tableau.node OpenComplete
        )
        z


switchBetas : Zipper -> Zipper
switchBetas z =
    modifyNode
        (\tableau ->
            case tableau.ext of
                Binary extType lt rt ->
                    Tableau tableau.node (Binary extType rt lt)

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


changeRule : (Tableau -> Tableau -> Maybe Extension) -> Zipper -> Zipper
changeRule extWithType z =
    if (z |> up) == z then
        z

    else
        modifyNode
            (\tableau ->
                -- pozor na koren
                case tableau.ext of
                    Open ->
                        tableau

                    Closed _ _ ->
                        tableau

                    OpenComplete ->
                        tableau

                    _ ->
                        case extWithType (Tableau.leftSubtree tableau) (Tableau.rightSubtree tableau) of
                            Nothing ->
                                tableau

                            Just extension ->
                                Tableau tableau.node extension
            )
            (z |> up)


doChangeToUnaryRule : (Tableau -> Extension) -> Zipper -> Zipper
doChangeToUnaryRule extWithType z =
    changeRule
        (\lt rt ->
            if Tableau.isEmpty lt then
                Just (extWithType rt)

            else if Tableau.isEmpty rt then
                Just (extWithType lt)

            else
                Nothing
        )
        z


changeToUnaryRule : UnaryExtType -> Zipper -> Zipper
changeToUnaryRule extType z =
    doChangeToUnaryRule (Unary extType) z


changeToUnaryRuleWithSubst : UnaryWithSubstExtType -> Zipper -> Zipper
changeToUnaryRuleWithSubst extType z =
    doChangeToUnaryRule (\t -> UnaryWithSubst extType t (zSubstitution (z |> up) |> Maybe.withDefault defSubstitution)) z


changeToBinaryRule : BinaryExtType -> Zipper -> Zipper
changeToBinaryRule extType z =
    changeRule (\t1 t2 -> Just (Binary extType t1 t2)) z


prettify : Tableau -> Tableau
prettify t =
    let
        z =
            zipper t

        prettifyNode : Tableau.Node -> Tableau.Node
        prettifyNode n =
            let
                newValue =
                    case Formula.Parser.parseSigned n.value of
                        Ok f ->
                            Formula.Signed.toString f

                        Err _ ->
                            n.value
            in
            { n | value = newValue }
    in
    z
        |> modifyNode
            (\tableau ->
                case tableau.ext of
                    Unary extType subT ->
                        Tableau (tableau.node |> prettifyNode) (Unary extType (prettify subT))

                    UnaryWithSubst extType subT subst ->
                        Tableau (tableau.node |> prettifyNode) (UnaryWithSubst extType (prettify subT) subst)

                    Binary extType lt rt ->
                        Tableau (tableau.node |> prettifyNode) (Binary extType (prettify lt) (prettify rt))

                    Open ->
                        Tableau (tableau.node |> prettifyNode) Open

                    Closed r1 r2 ->
                        Tableau (tableau.node |> prettifyNode) (Closed r1 r2)

                    OpenComplete ->
                        Tableau (tableau.node |> prettifyNode) OpenComplete
            )
        |> zTableau
