module Editor exposing (..)
import Html exposing (Html, Attribute, div, input, button, table, tr, td, text, pre)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Result
import Tableau

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


top = Tableau.top >> Tableau.zTableau
topRenumbered = top >> Tableau.renumber

update : Msg -> Model -> Model
update msg model =
  case msg of
    ExpandAlpha z -> { model | t = z |> Tableau.extendAlpha    |> topRenumbered }
    ExpandBeta  z -> { model | t = z |> Tableau.extendBeta     |> topRenumbered }
    MakeClosed  z -> { model | t = z |> Tableau.makeClosed     |> top }
    MakeOpen    z -> { model | t = z |> Tableau.makeOpen       |> top }
    Delete      z -> { model | t = z |> Tableau.delete         |> topRenumbered }
    Text    z txt -> { model | t = z |> Tableau.setFormula txt |> top }
    Ref     z ref -> { model | t = z |> Tableau.setRef (
      ref |> String.toInt |> Result.withDefault 0
    ) |> top }
    SetClosed which z ref -> { model | t = z |> Tableau.setClosed which (
      ref |> String.toInt |> Result.withDefault 0
    ) |> top }


errorColor res =
  case res of
    Err e -> if e.source /= "" then "lightpink" else "white"
    Ok _ -> "white"

view : Model -> Html Msg
view model =
  div []
    [ viewTableau model.t
    , pre []
      [ text (Tableau.indented 2 model.t)
      ]
    , div []
      [ text (toString model.t) ]
    ]



viewTableau : Tableau.Tableau -> Html Msg
viewTableau tbl=
  let
      t = Tableau.asTable tbl
  in
--     table [border "1"] (List.map tblRow t)
      table [] (List.map2 tblRow
        (List.reverse <| List.range 1 (List.length t))
        t
      )

border = attribute "border"
valign = attribute "valign"

tblRow : Int -> Tableau.Row -> (Html Msg)
tblRow depth trow =
  tr [] (List.map (tblCell depth) trow)

tblCell : Int -> Tableau.Cell -> (Html Msg)
tblCell depth tcell =
  let
      (width, mz) = tcell
      (content,height) = case mz of
        Nothing -> ([],depth)
        Just z ->
          ( [ viewFormula z ] ++ expandControls z
          , let
              (t, bs) = z
            in
              case t of
                Tableau.Leaf _ _ -> depth + 1
                _ -> 1
          )
  in td
    [ colspan (width), rowspan height, valign "top" ]
    content

-- TODO real css from outside
viewFormula z =
  let
    (t, bs) = z
    n = Tableau.node t
    formula = Formula.parseSigned n.text
  in
    div [ title (toString formula) ]
      [ div [ style [("display", "flex")] ]
        [ text <| "(" ++ (toString n.num) ++ ")"
        , input
            [ type_ "text", placeholder "Formula"
            , onInput <| Text z
            , style
              [ ("background-color", errorColor formula)
              , ("flexGrow", "1")
              , ("textAlign", "center")
              ]
            ]
            []
        , text " ["
        , input
          [ type_ "text", placeholder "0", size 1, onInput <| Ref z ]
          []
        , text "]"
        , button [ onClick (Delete z) ] [ text "x" ]
        ]
      ]


expandControls z =
  let
    (t, bs) = z
  in
    case t of
      Tableau.Leaf n mc -> case mc of
        Nothing ->
          [ div [style [("textAlign", "center")]]
            [ button [ onClick (ExpandAlpha z) ] [ text "α" ]
            , button [ onClick (ExpandBeta  z) ] [ text "β" ]
            , button [ onClick (MakeClosed  z) ] [ text "*" ]
            ]
          ]
        Just (a,b) ->
          [ div [style [("textAlign", "center")]]
            [ text "* "
            , input [ type_ "text", placeholder "0", size 1, onInput <| SetClosed 0 z] []
            , input [ type_ "text", placeholder "0", size 1, onInput <| SetClosed 1 z] []
            , button [ onClick (MakeOpen z) ] [ text "x" ]
            ]
          ]
      Tableau.Alpha _ _ -> []
      Tableau.Beta _ _ _ -> []

