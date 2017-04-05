import Html exposing (Html, Attribute, div, input, table, tr, td, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Result
import Parser
import Tableau

import Formula exposing (Formula)

main = Html.beginnerProgram { model = model, view = view, update = update }

type alias Model =
  { formulaTxt : String
  }

model  : Model
model = Model ""
type Msg
  = FChanged String

update : Msg -> Model -> Model
update msg model =
  case msg of
    FChanged formulaTxt ->
      { model | formulaTxt = formulaTxt }


errorColor res =
  case res of
    Err e -> if e.source /= "" then "lightpink" else "white"
    Ok _ -> "white"

view : Model -> Html Msg
view model =
  let
    formula = Formula.parse model.formulaTxt
  in
    div []
      [ input
          [ type_ "text", placeholder "Formula", onInput FChanged
          , style[("background-color", errorColor formula)]
          ]
          []
      , div [] [text (toString formula) ]
      , viewTableau Tableau.tt
      ]

viewTableau : Tableau.Tableau -> Html Msg
viewTableau tblx=
  let
      t = Tableau.asTable tblx
  in
     table [border "1"] (List.map tblRow t)

border = attribute "border"

tblRow : Tableau.Row -> (Html Msg)
tblRow trow =
  tr [] (List.map tblCell trow)

tblCell : Tableau.Cell -> (Html Msg)
tblCell tcell =
  td [ colspan (Tuple.first tcell) ] [text (Tableau.node (Tuple.second tcell)).text ]
