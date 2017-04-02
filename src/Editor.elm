module Editor exposing (..)
import Html exposing (Html, Attribute, div, input, button, table, tr, td, text, pre, p, ul, li)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Result
import Tableau exposing (..)
import Validate exposing (..)
import Errors

import Formula exposing (Formula)

main = Html.beginnerProgram { model = model, view = view, update = update }

type alias Model =
  { t : Tableau.Tableau
  }

model  : Model
model = Model (Tableau.fLeaf "")

type Msg
  = Text Tableau.Zipper String
  | Ref Tableau.Zipper String
  | ExpandAlpha Tableau.Zipper
  | ExpandBeta Tableau.Zipper
  | MakeClosed Tableau.Zipper
  | MakeOpen Tableau.Zipper
  | SetClosed Int Tableau.Zipper String
  | Delete Tableau.Zipper
  | Prettify


top = Tableau.top >> Tableau.zTableau
topRenumbered = top >> Tableau.renumber

update : Msg -> Model -> Model
update msg model =
  case msg of
    ExpandAlpha     z -> { model | t = z |> Tableau.extendAlpha     |> topRenumbered }
    ExpandBeta      z -> { model | t = z |> Tableau.extendBeta      |> topRenumbered }
    MakeClosed      z -> { model | t = z |> Tableau.makeClosed      |> top }
    MakeOpen        z -> { model | t = z |> Tableau.makeOpen        |> top }
    Delete          z -> { model | t = z |> Tableau.delete          |> topRenumbered }
    Text        z txt -> { model | t = z |> Tableau.setFormula txt  |> top }
    Ref         z ref -> { model | t = z |> Tableau.setRef ref      |> top }
    SetClosed w z ref -> { model | t = z |> Tableau.setClosed w ref |> top }
    Prettify          -> { model | t = Tableau.prettify model.t }


view : Model -> Html Msg
view model =
  div []
    [ viewTableau model.t
    , div []
      [ p [] [text "Problems"]
      , problemList <| Errors.errors <| isCorectTableau <| Tableau.zipper <| model.t
      ]
    , p [] [ button [ onClick Prettify ] [text "Prettify formulas"] ]
    , div []
      ( if False
        then
          [ p [] [text "Debug"]
          , pre []
            [ text (Tableau.indented 2 model.t)
            ]
          , p []
            [ text (toString model.t) ]
          ]
        else
        []
      )
    ]


problemList : (List Validate.Problem) -> Html Msg
problemList pl = ul [ class "problemList" ] (List.map problemItem pl)

problemItem : Validate.Problem -> Html Msg
problemItem pi = li [ class (problemClass pi) ]
  [ text "("
  , text <| toString <| .num <| Tableau.zNode <| pi.zip
  , text ") "
  , text <| pi.msg
  ]

problemsClass : (List Validate.Problem) -> String
problemsClass pl =
  case pl of
    [] -> ""
    p::_ -> problemClass p

errorClass : Result (List Validate.Problem) a -> String
errorClass =
  Errors.errors >> problemsClass

problemClass { typ } =
  case typ of
    Validate.Syntax    -> "syntaxProblem"
    Validate.Semantics -> "semanticsProblem"

problemColor : Validate.Problem -> String
problemColor p =
  case p.typ of
    Validate.Syntax -> "lightpink"
    Validate.Semantics -> "yellow"

viewTableau : Tableau.Tableau -> Html Msg
viewTableau tbl=
  let
      t = Tableau.asTable tbl
  in
      table [class "tableau"] (List.map2 tblRow
        (List.reverse <| List.range 1 (List.length t))
        t
      )

tblRow : Int -> Tableau.Row -> (Html Msg)
tblRow depth trow =
  tr [] (List.map (tblCell depth) trow)

tblCell : Int -> Tableau.Cell -> (Html Msg)
tblCell depth tcell =
  let
      (width, mz) = tcell
      (content,height,ttl) = case mz of
        Nothing -> ([],depth,"")
        Just z ->
          ( [ viewFormula z ] ++ expandControls z
          , let
              (t, bs) = z
            in
              case t of
                Tableau.Leaf _ _ -> depth + 1
                _ -> 1
          , ( z |> isCorrectNode |> Errors.errors
              |> List.map .msg |> String.join " \n "
            )
          )
  in td
    [ colspan width, rowspan height
    , title ttl
    ]
    content

viewFormula z =
  let
    (t, bs) = z
    n = Tableau.node t
    formula = Formula.parseSigned n.text
  in
    div
      [ class "formula"
      ]
      [ text <| "(" ++ (toString n.num) ++ ")"
      , input
        [ class ("formulaEdit "
            ++ errorClass (isCorrectFormula z)
            )
        , type_ "text"
        , placeholder "Formula"
        , value n.text
        , onInput <| Text z
        ]
        []
      , text " ["
      , input
        [ class ("refEdit " ++ errorClass (isValidNodeRef z))
        , type_ "text"
        , size 1
        , value n.ref.str
        , onInput <| Ref z
        ]
        []
      , text "]"
      , button [ onClick (Delete z) ] [ text "x" ]
      ]

expandControls z =
  let
    (t, bs) = z
  in
    [ div [ class "expandControls"]
      (case t of
        Tableau.Leaf n mc -> case mc of
          Nothing ->
            [ button [ onClick (ExpandAlpha z) ] [ text "α" ]
            , button [ onClick (ExpandBeta  z) ] [ text "β" ]
            , button [ onClick (MakeClosed  z) ] [ text "*" ]
            ]
          Just (r1,r2) ->
            let
              compl = Errors.errors <| areCloseRefsComplementary r1 r2 z
              ref1Cls = problemsClass <| (validateRef "Invalid close ref. #1" r1 z) ++ compl
              ref2Cls = problemsClass <| (validateRef "Invalid close ref. #1" r2 z) ++ compl
            in
              [ text "* "
              , input [ class ("refEdit " ++ ref1Cls), type_ "text"
                , placeholder "Ref", size 1, value r1.str, onInput <| SetClosed 0 z] []
              , input [ class ("refEdit " ++ ref2Cls), type_ "text"
                , placeholder "Ref", size 1, value r2.str, onInput <| SetClosed 1 z] []
              , button [ onClick (MakeOpen z) ] [ text "x" ]
              ]
        Tableau.Alpha _ _ -> []
        Tableau.Beta _ _ _ -> []
      )
    ]

{- vim: set sw=2 ts=2 sts=2 et : -}
