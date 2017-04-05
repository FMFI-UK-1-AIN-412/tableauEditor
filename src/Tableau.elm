module Tableau exposing (..)
import Formula exposing (Formula, Signed (T, F))
import Result exposing (Result (Ok, Err))
import Parser

type alias Node =
  { num : Int
  , text : String
  , ref : Int
  }

type Tableau
  = Leaf Node
  | Alpha Node Tableau
  | Beta Node Tableau Tableau

node : Tableau -> Node
node t =
  case t of
    Leaf n -> n
    Alpha n _ -> n
    Beta n _ _ -> n

formula : Tableau ->  Result Parser.Error (Signed Formula)
formula t =
  (node t).text
  |> Formula.parseSigned



fLeaf text = Leaf { num = 1, text = text, ref = 1 }
fAlpha text ct =
  let
    nt = mapNode (\n -> {n | num = n.num + 1}) ct
  in
    Alpha { num = 1, text = text, ref = 1} nt
fBeta text lt rt =
  let
    nlt = mapNode (\n -> {n | num = n.num + 1}) lt
    nrt = mapNode (\n -> {n | num = n.num + (maxNum nlt)}) rt
  in
    Beta { num = 1, text = text, ref = 1} nlt nrt

maxNum : Tableau -> Int
maxNum t =
  case t of
    Leaf n -> n.num
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
    Leaf n -> if n.num == num then extender n else Leaf n

extendAlpha = extend alphaExtender
extendBeta = extend betaExtender

alphaExtender : Int -> Node -> Tableau
alphaExtender maxNum n =
  Alpha
    n
    (Leaf { num = maxNum + 1, text = "", ref = 0})

betaExtender : Int -> Node -> Tableau
betaExtender maxNum n =
  Beta
    n
    (Leaf { num = maxNum + 1, text = "", ref = 0})
    (Leaf { num = maxNum + 2, text = "", ref = 0})

mapNode : (Node -> Node) -> Tableau -> Tableau
mapNode f t =
  case t of
    Leaf n -> Leaf <| f n
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
    (String.repeat ind " ") ++ parsedFrm ++ " [" ++ (toString n.ref) ++ "]\n"

indented ind t =
  case t of
    Leaf n -> indentedNode ind n
    Alpha n ct ->
      (indentedNode ind n) ++ (indented (ind + 2) ct)
    Beta n lt rt ->
      (indentedNode ind n) ++ (indented (ind + 2) lt) ++ (indented (ind + 2) rt)

width : Tableau -> Int
width t =
  case t of
    Leaf _ -> 1
    Alpha _ t -> width t
    Beta _ lt rt -> (width lt) + (width rt)

type alias CellWidth = Int
type alias Cell = (CellWidth, Tableau) -- the 'Node' at that point
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
    Leaf n -> ( (1, t), [] )
    Alpha n ct -> let
                    (top, table) = asHeadedTable ct
                    (topWidth, topElem) = top
                 in
                    (  ( topWidth, t), [[top]] ++ table)
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
         ( (ltopWidth + rtopWidth, t)
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
