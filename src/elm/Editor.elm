module Editor exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Parser
import Tableau exposing (..)
import Zipper exposing (..)
import Rules exposing (..)
import Errors
import Formula
import Helper


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


type alias Model =
    { tableau : Tableau
    }


init : ( Model, Cmd msg )
init =
    ( { tableau =
            { node =
                { id = 1
                , value = ""
                , reference = { str = "1", up = Just 0 }
                , formula = Formula.parseSigned ""
                }
            , ext = Open
            }
      }
    , Cmd.none
    )


type Msg
    = ChangeText Zipper.Zipper String
    | ChangeRef Zipper.Zipper String
    | ExpandAlpha Zipper.Zipper
    | ExpandBeta Zipper.Zipper
    | Delete Zipper.Zipper
    | MakeClosed Zipper.Zipper


top : Zipper.Zipper -> Tableau
top =
    Zipper.top >> Zipper.zTableau


topRenumbered : Zipper.Zipper -> Tableau
topRenumbered =
    top >> Zipper.renumber


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    Debug.log "model"
        (case msg of
            ChangeText z new ->
                ( { model | tableau = (z |> Zipper.setFormula new |> top) }, Cmd.none )

            ExpandAlpha z ->
                ( { model | tableau = (z |> Zipper.extendAlpha |> topRenumbered) }, Cmd.none )

            ExpandBeta z ->
                ( { model | tableau = (z |> Zipper.extendBeta |> topRenumbered) }, Cmd.none )

            ChangeRef z new ->
                ( { model | tableau = (z |> Zipper.setRef new |> top) }, Cmd.none )

            Delete z ->
                ( { model | tableau = (z |> Zipper.delete |> topRenumbered) }, Cmd.none )

            MakeClosed z ->
                ( { model | tableau = (z |> Zipper.makeClosed |> top) }, Cmd.none )
        )


view : Model -> Html Msg
view model =
    div [ class "tableau" ]
        [ viewNode (Zipper.zipper model.tableau)
        , Rules.help
        ]


viewNode : Zipper.Zipper -> Html Msg
viewNode z =
    let
        ( tableau, bs ) =
            z
    in
        div
            [ class "formula" ]
            [ text <| "(" ++ ((Zipper.zNode z).id |> toString) ++ ")"
            , input
                [ classList
                    [ ( "formulaInput", True )
                    , ( "premise", Helper.isPremise z )
                    , ( "semanticsProblem", Helper.hasReference z )
                    ]
                , value (Zipper.zNode z).value
                , type_ "text"
                , onInput <| ChangeText z
                ]
                []
            , text "["
            , input
                [ class "formulaReference"
                , value (Zipper.zNode z).reference.str
                , onInput <| ChangeRef z

                --                , size ((String.length tableau.node.value) * 3 // 4 + 1)
                ]
                []
            , text "]"
            , button [ class "delete", onClick (Delete z) ] [ text "x" ]
            , viewChildren z
            ]


viewChildren : Zipper.Zipper -> Html Msg
viewChildren z =
    case (Zipper.zTableau z).ext of
        Tableau.Open ->
            viewOpen z

        Tableau.Closed r1 r2 ->
            div [] [ text "*" ]

        Tableau.Alpha t ->
            viewAlpha z

        Tableau.Beta lt rt ->
            viewBeta z


viewAlpha : Zipper.Zipper -> Html Msg
viewAlpha z =
    div [ class "alpha" ] [ viewNode (Zipper.down z) ]


viewBeta : Zipper.Zipper -> Html Msg
viewBeta z =
    div [ class "beta" ]
        [ viewNode (Zipper.left z)
        , viewNode (Zipper.right z)
        ]


viewOpen : Zipper.Zipper -> Html Msg
viewOpen z =
    div [ class "open" ]
        [ expandControls z
        ]


expandControls : Zipper.Zipper -> Html Msg
expandControls z =
    let
        ( t, bs ) =
            z
    in
        div [ class "expandControls" ]
            (case t.ext of
                Tableau.Open ->
                    [ button [ onClick (ExpandAlpha z) ] [ text "α" ]
                    , button [ onClick (ExpandBeta z) ] [ text "β" ]
                    , button [ class "delete", onClick (MakeClosed z) ] [ text "*" ]
                    ]

                Tableau.Alpha _ ->
                    []

                Tableau.Beta _ _ ->
                    []

                Tableau.Closed _ _ ->
                    []
            )
