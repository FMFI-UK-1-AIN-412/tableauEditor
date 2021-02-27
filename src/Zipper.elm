module Zipper exposing (..)

import Debug exposing (log)
import Formula
import Formula.Parser
import Formula.Signed
import Html exposing (table)
import Tableau exposing (..)



--crumb hovori o tom, kto je podo mnou


type Crumb
    = UnaryCrumb ExtType Node
    | UnaryCrumbWithSubst ExtType Node Tableau.Substitution
    | BinaryLeftCrumb ExtType Node Tableau
    | BinaryRightCrumb ExtType Node Tableau

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
        Unary _ _ ->
            [down z]

        UnaryWithSubst _ _ _ ->
            [down z]

        Binary _ _ _ ->
            [ left z, right z ]

        _ ->
            []


down : Zipper -> Zipper
down ( t, bs ) =
    case t.ext of
        Unary extType subT ->
            ( subT, UnaryCrumb extType t.node :: bs )

        UnaryWithSubst extType subT subst ->
            ( subT, UnaryCrumbWithSubst extType t.node subst :: bs )

        _ ->
            ( t, bs )


right : Zipper -> Zipper
right ( t, bs ) =
    case t.ext of
        Binary extType lt rt ->
            ( rt, BinaryRightCrumb extType t.node lt :: bs )

        _ ->
            ( t, bs )



--vracia zipper pre mojho laveho syna


left : Zipper -> Zipper
left ( t, bs ) =
    case t.ext of
        Binary extType lt rt ->
            ( lt, BinaryLeftCrumb extType t.node rt :: bs )

        _ ->
            ( t, bs )


up : Zipper -> Zipper
up ( t, bs ) =
    case bs of
        (UnaryCrumb extType n) :: bss ->
            ( Tableau n (Unary extType t), bss )

        (UnaryCrumbWithSubst extType n subst) :: bss ->
            ( Tableau n (UnaryWithSubst extType t subst), bss )

        (BinaryLeftCrumb extType n rt) :: bss ->
            (Tableau n (Binary extType t rt), bss)

        (BinaryRightCrumb extType n lt) :: bss ->
            (Tableau n (Binary extType lt t), bss)

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
        UnaryWithSubst _ _ subst ->
            Just subst

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
        renumberUnary extWithType = 
            let
                ( new_tableau, num1 ) =
                    renumber2 (Tableau.leftSubtree tableau) (num + 1)

                node =
                    tableau.node
            in
            ( Tableau { node | id = num + 1 } (extWithType new_tableau), num1 ) 

        renumberBinary extWithType =
            let
                ( new_left, num1 ) =
                    renumber2 (Tableau.leftSubtree tableau) (num + 1)

                ( new_right, num2 ) =
                    renumber2 (Tableau.rightSubtree tableau) num1

                node =
                    tableau.node
            in
            ( Tableau { node | id = num + 1 } (extWithType new_left new_right), num2 )   
    in
    
    case tableau.ext of
        Open ->
            let
                node =
                    tableau.node

                ext =
                    tableau.ext
            in
            ( Tableau { node | id = num + 1 } ext, num + 1 )

        Closed r1 r2 ->
            let
                node =
                    tableau.node

                ext =
                    tableau.ext
            in
            ( Tableau { node | id = num + 1 } ext, num + 1 )

        Unary extType _ ->
            renumberUnary (Unary extType)

        UnaryWithSubst extType _ subst ->
            renumberUnary (\t -> (UnaryWithSubst extType) t subst) 

        Binary extType _ _ ->
            renumberBinary (Binary extType)


modifyRef : List Ref -> Zipper -> Zipper
modifyRef refs z =
    modifyNode
        (\({ node } as tableau) ->
            { tableau | node = { node | references = refs } }
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


getRef : Zipper -> String -> Ref
getRef z ref =
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
        renumberJustsUnary extWithType =
            Tableau
                tableau.node
                (extWithType (renumberJusts (renumberJust (Tableau.leftSubtree tableau) f (lengthOfPathFromFather + 1)) f (lengthOfPathFromFather + 1)))

        renumberJustsBinary extWithType =
            Tableau
                tableau.node
                (extWithType
                    (renumberJusts (renumberJust (Tableau.leftSubtree tableau) f (lengthOfPathFromFather + 1)) f (lengthOfPathFromFather + 1))
                    (renumberJusts (renumberJust (Tableau.rightSubtree tableau) f (lengthOfPathFromFather + 1)) f (lengthOfPathFromFather + 1))
                )
    in
    
    case tableau.ext of
        Unary extType _ ->
            renumberJustsUnary (Unary extType)

        UnaryWithSubst extType _ subst ->
            renumberJustsUnary (\t -> UnaryWithSubst extType t subst)

        Binary extType _ _ ->
            renumberJustsBinary (Binary extType)

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


extendWithRule : (Tableau -> Extension) -> Zipper -> Zipper
extendWithRule extWithType z =
    z
        |> modifyNode
            (\tableau ->
                Tableau (closeControls tableau.node) (extWithType (Tableau defNode tableau.ext))
            )


extendRuleWithSubst : (Tableau -> Substitution -> Extension) -> Zipper -> Zipper
extendRuleWithSubst extWithType z =
    extendWithRule (\t -> extWithType t defSubstitution) z


extendAlpha : Zipper -> Zipper
extendAlpha z =
    extendWithRule (Unary Alpha) z


extendBeta : Zipper -> Zipper
extendBeta z =
    extendWithRule (\t -> (Binary Beta) t (Tableau defNode Open)) z


extendGamma : Zipper -> Zipper
extendGamma z =
    extendRuleWithSubst (UnaryWithSubst Gamma) z


extendDelta : Zipper -> Zipper
extendDelta z =
    extendRuleWithSubst (UnaryWithSubst Delta) z


extendRefl : Zipper -> Zipper
extendRefl z =
    extendWithRule (Unary Refl) z


extendLeibnitz : Zipper -> Zipper
extendLeibnitz z =
    extendWithRule (Unary Leibnitz) z

extendMP : Zipper -> Zipper
extendMP z =
    extendWithRule (Unary MP) z

extendMT : Zipper -> Zipper
extendMT z =
    extendWithRule (Unary MT) z

extendCut : Zipper -> Zipper
extendCut z =
    extendWithRule (\t -> (Binary Cut) t (Tableau defNode Open)) z

extendHS : Zipper -> Zipper
extendHS z =
    extendWithRule (Unary HS) z

extendDS : Zipper -> Zipper
extendDS z =
    extendWithRule (Unary DS) z

extendNCS : Zipper -> Zipper
extendNCS z =
    extendWithRule (Unary NCS) z


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

                    Closed r1 r2 ->
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
        
        case fatherbs of
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

                            Closed r1 r2 ->
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
                UnaryWithSubst extType t subst ->
                    Tableau tableau.node (UnaryWithSubst extType t { subst | var = newVariable })

                _ ->
                    tableau
        )
        (z |> up)


changeTerm : String -> Zipper -> Zipper
changeTerm newTerm z =
    modifyNode
        (\tableau ->
            case tableau.ext of
                UnaryWithSubst extType t subst ->
                    Tableau tableau.node (UnaryWithSubst extType t { subst | term = newTerm })

                _ ->
                    tableau
        )
        (z |> up)


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
changeRule extType z =
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

                    _ ->
                        case extType (Tableau.leftSubtree tableau) (Tableau.rightSubtree tableau) of
                            Nothing ->
                                tableau

                            Just extension ->
                                Tableau tableau.node extension
            )
            (z |> up)


changeToUnaryRule : (Tableau -> Extension) -> Zipper -> Zipper
changeToUnaryRule extWithType z =
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


changeToUnaryRuleWithSubst : (Tableau -> Substitution -> Extension) -> Zipper -> Zipper
changeToUnaryRuleWithSubst extWithType z =
    changeToUnaryRule (\t -> extWithType t (zSubstitution (z |> up) |> Maybe.withDefault defSubstitution)) z


changeToAlpha : Zipper -> Zipper
changeToAlpha z =
    changeToUnaryRule (Unary Alpha) z 


changeToBeta : Zipper -> Zipper
changeToBeta z =
    changeRule (\t1 t2 -> Just ((Binary Beta) t1 t2)) z 


changeToGamma : Zipper -> Zipper
changeToGamma z =
    changeToUnaryRuleWithSubst (UnaryWithSubst Gamma) z


changeToDelta : Zipper -> Zipper
changeToDelta z =
    changeToUnaryRuleWithSubst (UnaryWithSubst Delta) z


changeToRefl : Zipper -> Zipper
changeToRefl z =
    changeToUnaryRule (Unary Refl) z


changeToLeibnitz : Zipper -> Zipper
changeToLeibnitz z =
    changeToUnaryRule (Unary Leibnitz) z

changeToMP : Zipper -> Zipper
changeToMP z =
    changeToUnaryRule (Unary MP) z

changeToMT : Zipper -> Zipper
changeToMT z =
    changeToUnaryRule (Unary MT) z

changeToCut : Zipper -> Zipper
changeToCut z =
    changeRule (\t1 t2 -> Just ((Binary Cut) t1 t2)) z

changeToHS : Zipper -> Zipper
changeToHS z =
    changeToUnaryRule (Unary HS) z

changeToDS : Zipper -> Zipper
changeToDS z =
    changeToUnaryRule (Unary DS) z

changeToNCS : Zipper -> Zipper
changeToNCS z =
    changeToUnaryRule (Unary NCS) z


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
            )
        |> zTableau
