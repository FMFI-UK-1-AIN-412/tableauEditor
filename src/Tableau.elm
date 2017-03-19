module Tableau exposing (..)
import Formula exposing (Formula, Signed (T, F))
import Result exposing (Result (Ok, Err))
import Parser

type alias Node =
  { num : Int
  , text : String
  , ref : Int
  }
defNode = { num = 1, text = "", ref = 1 }

type alias Closed = (Int, Int)
defClosed = (0, 0)

type Tableau
  = Leaf Node (Maybe Closed)
  | Alpha Node Tableau
  | Beta Node Tableau Tableau

node : Tableau -> Node
node t =
  case t of
    Leaf n _ -> n
    Alpha n _ -> n
    Beta n _ _ -> n

formula : Tableau ->  Result Parser.Error (Signed Formula)
formula t =
  (node t).text
  |> Formula.parseSigned


depth : Tableau -> Int
depth t = case t of
  Leaf _ _ -> 1
  Alpha _ t -> 1 + depth t
  Beta _ lt rt -> 1 + depth lt + depth rt

fLeaf text = Leaf { defNode | text = text } Nothing
fAlpha text ct =
  let
    nt = mapNode (\n -> {n | num = n.num + 1}) ct
  in
    Alpha { defNode | text = text } nt
fBeta text lt rt =
  let
    nlt = mapNode (\n -> {n | num = n.num + 1}) lt
    nrt = mapNode (\n -> {n | num = n.num + (maxNum nlt)}) rt
  in
    Beta { defNode | text = text } nlt nrt

maxNum : Tableau -> Int
maxNum t =
  case t of
    Leaf n _ -> n.num
    Alpha n ct -> max n.num (maxNum ct)
    Beta n lt rt -> max n.num (max (maxNum lt) (maxNum rt))

extend : (Int -> Node -> Tableau) -> Int -> Tableau -> Tableau
extend extender num t = doExtend (extender (maxNum t)) num t

doExtend : (Node -> Tableau) -> Int -> Tableau -> Tableau
doExtend extender num t =
  case t of
    Alpha n t -> Alpha n <| doExtend extender num t
    Beta n lt rt ->
      Beta
        n
        ( doExtend extender num lt )
        ( doExtend extender num rt )
    Leaf n _ -> if n.num == num then extender n else Leaf n Nothing

extendAlpha = extend alphaExtender
extendBeta = extend betaExtender

alphaExtender : Int -> Node -> Tableau
alphaExtender maxNum n =
  Alpha
    n
    (Leaf { defNode | num = maxNum + 1 } Nothing)

betaExtender : Int -> Node -> Tableau
betaExtender maxNum n =
  Beta
    n
    (Leaf { defNode | num = maxNum + 1 } Nothing)
    (Leaf { defNode | num = maxNum + 2 } Nothing)

mapTableau : (Tableau -> Tableau) -> Tableau -> Tableau
mapTableau f t =
  case t of
    Leaf _ _ -> f t
    Alpha n st -> f (Alpha n (mapTableau f st))
    Beta n lt rt -> f (Beta n (mapTableau f lt) (mapTableau f rt))

leafMapper : (Node -> (Maybe Closed) -> Tableau) ->  Tableau -> Tableau
leafMapper f t =
  case t of
    Leaf n mt -> f n mt
    _ -> t
mapLeaves : (Node -> (Maybe Closed) -> Tableau) -> Tableau -> Tableau
mapLeaves f = mapTableau (leafMapper f)
modifyLeaf : (Node -> (Maybe Closed) -> Tableau) -> Int -> Tableau -> Tableau
modifyLeaf f num =
  mapLeaves (\n mc -> if n.num == num then (f n mc) else (Leaf n mc))

setPair1 : Int -> (a,a) -> a -> (a,a)
setPair1 which p n =
  let
    (a,b) = p
  in
    case which of
      0 -> (n,b)
      _ -> (a,n)

setClosed : Int -> Int -> Int -> Tableau -> Tableau
setClosed which ref =
  modifyLeaf (\n mc ->
    case mc of
      Just (a,b) -> Leaf n (Just (setPair1 which (a,b) ref))
      _ -> Leaf n mc
    )



makeClosed : Int -> Tableau -> Tableau
makeClosed =
  modifyLeaf (\n mc -> case mc of
    Nothing -> Leaf n (Just defClosed)
    Just _ as closed -> Leaf n closed
  )

mapNode : (Node -> Node) -> Tableau -> Tableau
mapNode f t =
  case t of
    Leaf n _ -> Leaf (f n) Nothing
    Alpha n ct -> Alpha (f n) (mapNode f ct)
    Beta n lt rt -> Beta (f n) (mapNode f lt) (mapNode f rt)

modifyNode : (Node -> Node) -> Int -> Tableau -> Tableau
modifyNode f num =
  mapNode (\n -> if n.num == num then f n else n)

setFormula frm = modifyNode (\x -> {x | text = frm})
setRef ref = modifyNode (\x -> {x | ref = ref})


indentedNode ind n =
  let
    parsedFrm = if n.text == "" then "" else case (Formula.parseSigned n.text) of
      Ok f -> Formula.strSigned f
      Err e -> toString e
  in
    (String.repeat ind " ") ++ "(" ++ toString n.num ++ ")"
    ++ parsedFrm ++ " [" ++ (toString n.ref) ++ "]"

indentedClosed mc =
  case mc of
    Nothing -> ""
    Just (a,b) -> "*(" ++ toString a ++ "," ++ toString b ++ ")"

indented ind t =
  case t of
    Leaf n mc -> indentedNode ind n ++ " " ++ indentedClosed mc ++ "\n"
    Alpha n ct ->
      (indentedNode ind n) ++ "\n" ++ (indented (ind + 2) ct)
    Beta n lt rt ->
      (indentedNode ind n) ++ "\n" ++ (indented (ind + 2) lt) ++ (indented (ind + 2) rt)

width : Tableau -> Int
width t =
  case t of
    Leaf _ _ -> 1
    Alpha _ t -> width t
    Beta _ lt rt -> (width lt) + (width rt)

type alias CellWidth = Int
type alias Cell = (CellWidth, Maybe Tableau) -- the 'Node' at that point
type alias Row = List Cell
type alias Table = List Row
asTable : Tableau -> Table
asTable t =
  let
    (c, tbl) = asHeadedTable t
  in
     [[c]] ++ tbl

asHeadedTable : Tableau -> (Cell, Table)
asHeadedTable t =
  case t of
    Leaf n _ -> ( (1, Just t), [] )
    Alpha n ct -> let
                    (top, table) = asHeadedTable ct
                    (topWidth, topElem) = top
                 in
                    (  ( topWidth, Just t), [[top]] ++ table)
--
-- TODO left needs to be extended with empty cells of correct widht if shorter
--
    Beta n lt rt ->
      let
        (ltop, ltable) = asHeadedTable lt
        (ltopWidth, ltopE) = ltop
        (rtop, rtable) = asHeadedTable rt
        (rtopWidth, rtopE) = rtop
      in
         ( (ltopWidth + rtopWidth, Just t)
         , [[ltop, rtop]]  ++ (merge ltable rtable)
         )


-- grr, no asymetric map2 ;(
merge : (List (List a)) -> (List (List a)) -> List (List a)
merge ll rl =
  case (ll,rl) of
    (lh::lt, rh::rt) -> (lh++rh) :: merge lt rt
    ([], rh::rt) -> rh :: merge [] rt
    (lh::lt, []) -> lh :: merge lt []
    ([], []) -> []

tl =
  fAlpha "T(a&b)"
   ( fAlpha "Ta"
       ( fAlpha "Tb"
          (fLeaf "Tc")
       )
   )

tt =
  fBeta "T((a&b)|c)"
    (fAlpha "F (a&b)"
      (fLeaf "Ta")
    )
    (fLeaf "Fc")
