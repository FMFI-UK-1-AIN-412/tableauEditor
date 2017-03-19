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
  = Text Int String
  | Ref Int String
  | ExpandAlpha Int
  | ExpandBeta Int
  | Close Int

update : Msg -> Model -> Model
update msg model =
  case msg of
    Text num txt ->    { model | t = (Tableau.setFormula txt num model.t) }
    Ref num ref ->
      case String.toInt ref of
        Ok ref -> { model | t = (Tableau.setRef ref num model.t) }
        Err e -> model
    ExpandAlpha num -> { model | t = (Tableau.extendAlpha num model.t) }
    ExpandBeta num ->  { model | t = (Tableau.extendBeta num model.t) }
    Close num -> model


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
    ]



viewTableau : Tableau.Tableau -> Html Msg
viewTableau tbl=
  let
      t = Tableau.asTable tbl
  in
--     table [border "1"] (List.map tblRow t)
      table [] (List.map2 tblRow
        (List.reverse <| List.range 1 (Tableau.depth tbl))
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
      (width, mt) = tcell
      (content,height) = case mt of
        Nothing -> ([],depth)
        Just t ->
          ( [ viewFormula t ] ++ expandControls t
          , case t of
            Tableau.Leaf _ _ -> depth + 1
            _ -> 1
          )
  in td
    [ colspan (width), rowspan height, valign "top" ]
    content

-- TODO real css from outside
viewFormula t =
  let
    n = Tableau.node t
    formula = Formula.parseSigned n.text
  in
    div [ title (toString formula) ]
      [ div [ style [("display", "flex")] ]
        [ text <| "(" ++ (toString n.num) ++ ")"
        , input
            [ type_ "text", placeholder "Formula"
            , onInput <| Text n.num
            , style
              [ ("background-color", errorColor formula)
              , ("flexGrow", "1")
              , ("textAlign", "center")
              ]
            ]
            []
        , text " ["
        , input
          [ type_ "text", placeholder "0", size 1, onInput <| Ref n.num ]
          []
        , text "]"
        ]
      ]


expandControls t =
  case t of
    Tableau.Leaf n _ -> [
      div [style [("textAlign", "center")]]
      [ button [ onClick (ExpandAlpha n.num) ] [ text "α" ]
      , button [ onClick (ExpandBeta n.num) ] [ text "β" ]
      , button [ onClick (Close n.num) ] [ text "*" ]
      ]
    ]
    Tableau.Alpha _ _ -> []
    Tableau.Beta _ _ _ -> []

