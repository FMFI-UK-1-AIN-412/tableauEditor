module Zipper exposing (..)

import Debug exposing (log)
import Formula
import Formula.Parser
import Formula.Signed
import Html exposing (table)
import Tableau exposing (..)



--crumb hovori o tom, kto je podo mnou


type Crumb
    = AlphaCrumb Node
    | BetaLeftCrumb Node Tableau
    | BetaRightCrumb Node Tableau
    | GammaCrumb Node Tableau.Substitution
    | DeltaCrumb Node Tableau.Substitution
    | ReflCrumb Node
    | LeibnitzCrumb Node
    | MPCrumb Node
    | MTCrumb Node
    | CutLeftCrumb Node Tableau
    | CutRightCrumb Node Tableau
    | HSCrumb Node
    | DSCrumb Node
    | NCSCrumb Node


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

        Refl _ ->
            [ down z ]

        Leibnitz _ ->
            [ down z ]

        MP _ ->
            [ down z ]

        MT _ ->
            [ down z ]
            
        Cut _ _ ->
            [ left z, right z ]
            
        HS _ ->
            [ down z ]
            
        DS _ ->
            [ down z ]
            
        NCS _ ->
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

        Refl subt ->
            ( subt, ReflCrumb t.node :: bs )

        Leibnitz subt ->
            ( subt, LeibnitzCrumb t.node :: bs )
        
        MP subt ->
            ( subt, MPCrumb t.node :: bs )
        
        MT subt ->
            ( subt, MTCrumb t.node :: bs )
        
        HS subt ->
            ( subt, HSCrumb t.node :: bs )
        
        DS subt ->
            ( subt, DSCrumb t.node :: bs )
        
        NCS subt ->
            ( subt, NCSCrumb t.node :: bs )

        _ ->
            ( t, bs )


right : Zipper -> Zipper
right ( t, bs ) =
    case t.ext of
        Beta tl tr ->
            ( tr, BetaRightCrumb t.node tl :: bs )

        Cut tl tr ->
            ( tr, CutRightCrumb t.node tl :: bs )

        _ ->
            ( t, bs )



--vracia zipper pre mojho laveho syna


left : Zipper -> Zipper
left ( t, bs ) =
    case t.ext of
        Beta tl tr ->
            ( tl, BetaLeftCrumb t.node tr :: bs )

        Cut tl tr ->
            ( tl, CutLeftCrumb t.node tr :: bs )

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

        (ReflCrumb n) :: bss ->
            ( Tableau n (Refl t), bss )

        (LeibnitzCrumb n) :: bss ->
            ( Tableau n (Leibnitz t), bss )

        (MPCrumb n) :: bss ->
            ( Tableau n (MP t), bss )

        (MTCrumb n) :: bss ->
            ( Tableau n (MT t), bss )

        (CutLeftCrumb n tr) :: bss ->
            ( Tableau n (Cut t tr), bss )

        (CutRightCrumb n tl) :: bss ->
            ( Tableau n (Cut tl t), bss )

        (HSCrumb n) :: bss ->
            ( Tableau n (HS t), bss )

        (DSCrumb n) :: bss ->
            ( Tableau n (DS t), bss )

        (NCSCrumb n) :: bss ->
            ( Tableau n (NCS t), bss )

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

        Beta tl tr ->
            z |> left |> zWalkPost f |> up |> right |> zWalkPost f |> up |> f
        
        Cut tl tr ->
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
        renumberUnary ext = 
            let
                ( new_tableau, num1 ) =
                    renumber2 (Tableau.leftSubtree tableau) (num + 1)

                node =
                    tableau.node
            in
            ( Tableau { node | id = num + 1 } (ext new_tableau), num1 ) 

        renumberBinary ext =
            let
                ( new_left, num1 ) =
                    renumber2 (Tableau.leftSubtree tableau) (num + 1)

                ( new_right, num2 ) =
                    renumber2 (Tableau.rightSubtree tableau) num1

                node =
                    tableau.node
            in
            ( Tableau { node | id = num + 1 } (ext new_left new_right), num2 )   
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

        Alpha _ ->
            renumberUnary Alpha

        Beta _ _ ->
            renumberBinary Beta

        Gamma _ subst ->
            renumberUnary (\t -> Gamma t subst)

        Delta _ subst ->
            renumberUnary (\t -> Delta t subst)

        Refl _ ->
            renumberUnary Refl

        Leibnitz _ ->
            renumberUnary Leibnitz

        
        MP _ ->
            renumberUnary MP

        
        MT _ ->
            renumberUnary MT

        
        Cut _ _ ->
            renumberBinary Cut

        
        HS _ ->
            renumberUnary HS

        
        DS _ ->
            renumberUnary DS

        
        NCS _ ->
            renumberUnary NCS

        Closed r1 r2 ->
            let
                node =
                    tableau.node

                ext =
                    tableau.ext
            in
            ( Tableau { node | id = num + 1 } ext, num + 1 )


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
        renumberJustsUnary ext =
            Tableau
                tableau.node
                (ext (renumberJusts (renumberJust (Tableau.leftSubtree tableau) f (lengthOfPathFromFather + 1)) f (lengthOfPathFromFather + 1)))

        renumberJustsBinary ext =
            Tableau
                tableau.node
                (ext
                    (renumberJusts (renumberJust (Tableau.leftSubtree tableau) f (lengthOfPathFromFather + 1)) f (lengthOfPathFromFather + 1))
                    (renumberJusts (renumberJust (Tableau.rightSubtree tableau) f (lengthOfPathFromFather + 1)) f (lengthOfPathFromFather + 1))
                )
    in
    
    case tableau.ext of
        Alpha _ ->
            renumberJustsUnary Alpha

        Beta _ _ ->
            renumberJustsBinary Beta

        Gamma _ s ->
            renumberJustsUnary (\t -> Gamma t s)

        Delta _ s ->
            renumberJustsUnary (\t -> Delta t s)

        Refl _ ->
            renumberJustsUnary Refl

        Leibnitz _ ->
            renumberJustsUnary Leibnitz

        MP _ ->
            renumberJustsUnary MP

        MT _ ->
            renumberJustsUnary MT
     
        Cut _ _ ->
            renumberJustsBinary Cut
        
        HS _ ->
            renumberJustsUnary HS
        
        DS _ ->
            renumberJustsUnary DS

        NCS _ ->
            renumberJustsUnary NCS

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
extendWithRule extType z =
    z
        |> modifyNode
            (\tableau ->
                Tableau (closeControls tableau.node) (extType (Tableau defNode tableau.ext))
            )


extendRuleWithSubst : (Tableau -> Substitution -> Extension) -> Zipper -> Zipper
extendRuleWithSubst extType z =
    extendWithRule (\t -> extType t defSubstitution) z


extendAlpha : Zipper -> Zipper
extendAlpha z =
    extendWithRule Tableau.Alpha z


extendBeta : Zipper -> Zipper
extendBeta z =
    extendWithRule (\t -> Beta t (Tableau defNode Open)) z


extendGamma : Zipper -> Zipper
extendGamma z =
    extendRuleWithSubst Tableau.Gamma z


extendDelta : Zipper -> Zipper
extendDelta z =
    extendRuleWithSubst Tableau.Delta z


extendRefl : Zipper -> Zipper
extendRefl z =
    extendWithRule Tableau.Refl z


extendLeibnitz : Zipper -> Zipper
extendLeibnitz z =
    extendWithRule Tableau.Leibnitz z

extendMP : Zipper -> Zipper
extendMP z =
    extendWithRule Tableau.MP z

extendMT : Zipper -> Zipper
extendMT z =
    extendWithRule Tableau.MT z

extendCut : Zipper -> Zipper
extendCut z =
    extendWithRule (\t -> Cut t (Tableau defNode Open)) z

extendHS : Zipper -> Zipper
extendHS z =
    extendWithRule Tableau.HS z

extendDS : Zipper -> Zipper
extendDS z =
    extendWithRule Tableau.DS z

extendNCS : Zipper -> Zipper
extendNCS z =
    extendWithRule Tableau.NCS z


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

                    Beta lt rt ->
                        -- mozem zmazat iba ked jedna z biet sa moze stat alfou
                        tableauToKeep tableau lt rt

                    Cut lt rt ->
                        tableauToKeep tableau lt rt

                    _ ->
                        Tableau.leftSubtree tableau
            )
            zip

    else
        let
            tryToKeepRightSubT currentT leftT rightT = 
                if leftT.node.value == "" then
                    Tableau currentT.node (Alpha rightT)

                else
                    currentT

            tryToKeepLeftSubT currentT leftT rightT = 
                if rightT.node.value == "" then
                    Tableau currentT.node (Alpha leftT)

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
            (BetaLeftCrumb fatherNode tr) :: bss ->
                chooseSubTableau tryToKeepRightSubT

            (BetaRightCrumb fatherNode tl) :: bss ->
                chooseSubTableau tryToKeepLeftSubT

            (CutLeftCrumb fatherNode tr) :: bss ->
                chooseSubTableau tryToKeepRightSubT

            (CutRightCrumb fatherNode tl) :: bss ->
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

                            Beta _ _ ->
                                tableau

                            Cut _ _ ->
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

                Cut lt rt ->
                    Tableau tableau.node (Cut rt lt)

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
changeToUnaryRule extType z =
    changeRule
        (\lt rt ->
            if Tableau.isEmpty lt then
                Just (extType rt)

            else if Tableau.isEmpty rt then
                Just (extType lt)

            else
                Nothing
        )
        z


changeToUnaryRuleWithSubst : (Tableau -> Substitution -> Extension) -> Zipper -> Zipper
changeToUnaryRuleWithSubst extType z =
    changeToUnaryRule (\t -> extType t (zSubstitution (z |> up) |> Maybe.withDefault defSubstitution)) z


changeToAlpha : Zipper -> Zipper
changeToAlpha z =
    changeToUnaryRule Tableau.Alpha z


changeToBeta : Zipper -> Zipper
changeToBeta z =
    changeRule (\t1 t2 -> Just (Tableau.Beta t1 t2)) z


changeToGamma : Zipper -> Zipper
changeToGamma z =
    changeToUnaryRuleWithSubst Tableau.Gamma z


changeToDelta : Zipper -> Zipper
changeToDelta z =
    changeToUnaryRuleWithSubst Tableau.Delta z


changeToRefl : Zipper -> Zipper
changeToRefl z =
    changeToUnaryRule Tableau.Refl z


changeToLeibnitz : Zipper -> Zipper
changeToLeibnitz z =
    changeToUnaryRule Tableau.Leibnitz z

changeToMP : Zipper -> Zipper
changeToMP z =
    changeToUnaryRule Tableau.MP z

changeToMT : Zipper -> Zipper
changeToMT z =
    changeToUnaryRule Tableau.MT z

changeToCut : Zipper -> Zipper
changeToCut z =
    changeRule (\t1 t2 -> Just (Tableau.Cut t1 t2)) z

changeToHS : Zipper -> Zipper
changeToHS z =
    changeToUnaryRule Tableau.HS z

changeToDS : Zipper -> Zipper
changeToDS z =
    changeToUnaryRule Tableau.DS z

changeToNCS : Zipper -> Zipper
changeToNCS z =
    changeToUnaryRule Tableau.NCS z


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
                    Alpha tbl ->
                        Tableau (tableau.node |> prettifyNode) (Alpha (prettify tbl))

                    Beta tl tr ->
                        Tableau (tableau.node |> prettifyNode) (Beta (prettify tl) (prettify tr))

                    Gamma tbl subs ->
                        Tableau (tableau.node |> prettifyNode) (Gamma (prettify tbl) subs)

                    Delta tbl subs ->
                        Tableau (tableau.node |> prettifyNode) (Delta (prettify tbl) subs)

                    Refl tbl ->
                        Tableau (tableau.node |> prettifyNode) (Refl (prettify tbl))

                    Leibnitz tbl ->
                        Tableau (tableau.node |> prettifyNode) (Leibnitz (prettify tbl))

                    MP tbl ->
                        Tableau (tableau.node |> prettifyNode) (MP (prettify tbl))

                    MT tbl ->
                        Tableau (tableau.node |> prettifyNode) (MT (prettify tbl))

                    Cut tl tr ->
                        Tableau (tableau.node |> prettifyNode) (Cut (prettify tl) (prettify tr))

                    HS tbl ->
                        Tableau (tableau.node |> prettifyNode) (HS (prettify tbl))

                    DS tbl ->
                        Tableau (tableau.node |> prettifyNode) (DS (prettify tbl))

                    NCS tbl ->
                        Tableau (tableau.node |> prettifyNode) (NCS (prettify tbl))

                    Open ->
                        Tableau (tableau.node |> prettifyNode) Open

                    Closed r1 r2 ->
                        Tableau (tableau.node |> prettifyNode) (Closed r1 r2)
            )
        |> zTableau
