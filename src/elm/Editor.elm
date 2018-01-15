module Editor exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Parser
import Tableau exposing (..)
import Zipper exposing (..)
import HelpersView exposing (..)
import Errors


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
    ( { tableau = { node = { id = 1, value = "", reference = { str = "1", up = Just 0 } }, ext = Open } }
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
    let
        _ =
            Debug.log "model" model
    in
        case msg of
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


view : Model -> Html Msg
view model =
    viewTableau model.tableau


viewTableau : Tableau.Tableau -> Html Msg
viewTableau tbl =
    let
        t =
            HelpersView.asTable tbl
    in
        table [ class "tableau" ]
            (List.map2 tblRow
                (List.reverse <| List.range 1 (List.length t))
                t
            )


tblRow : Int -> HelpersView.Row -> Html Msg
tblRow depth trow =
    tr [] (List.map (tblCell depth) trow)


tblCell : Int -> HelpersView.Cell -> Html Msg
tblCell depth tcell =
    let
        ( width, mz ) =
            tcell

        ( content, height, clss, ttl ) =
            case mz of
                Nothing ->
                    ( [], depth, [], "" )

                Just z ->
                    ( [ viewNode z ] ++ expandControls z
                    , let
                        ( t, bs ) =
                            z
                      in
                        case t.ext of
                            Tableau.Open ->
                                depth + 1

                            _ ->
                                1
                    , [ ( "premise", isPremise z ), ( "beta", isBeta z ) ]
                    , "sdjfhgskdhgksdhkldsfjhg"
                      --                    , (z
                      --                        |> isCorrectNode
                      --                        |> Errors.errors
                      --                        |> List.map .msg
                      --                        |> String.join " \n "
                      --                      )
                    )
    in
        td
            [ classList clss
            , colspan width
            , rowspan height
            , title ttl
            ]
            content


viewNode : Zipper.Zipper -> Html Msg
viewNode z =
    let
        ( tableau, bs ) =
            z
    in
        div [ class "formula" ]
            --            text |< --> todo
            [ input [ size ((String.length tableau.node.value) * 3 // 4 + 1), value ((Zipper.zNode z).id |> toString) ] []
            , input [ class "formulaEdit ", value (Zipper.zNode z).value, type_ "text", onInput <| ChangeText z ] []
            , text "["
            , input
                [ class "refEdit "
                , value (Zipper.zNode z).reference.str
                , size ((String.length tableau.node.value) * 3 // 4 + 1)
                , onInput <| ChangeRef z
                ]
                []
            , text "]"
            , button [ class "delete", onClick (Delete z) ] [ text "x" ]
            ]


expandControls : Zipper.Zipper -> List (Html Msg)
expandControls z =
    let
        ( t, bs ) =
            z
    in
        [ div [ class "expandControls" ]
            (case t.ext of
                Tableau.Open ->
                    [ button [ onClick (ExpandAlpha z) ] [ text "α" ]
                    , button [ onClick (ExpandBeta z) ] [ text "β" ]
                    , button [ onClick (MakeClosed z) ] [ text "*" ]
                    ]

                Tableau.Alpha _ ->
                    []

                Tableau.Beta _ _ ->
                    []

                Tableau.Closed _ _ ->
                    []
            )
        ]
