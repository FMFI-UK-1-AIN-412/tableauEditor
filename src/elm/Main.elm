module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Parser
import Zipper exposing (..)


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


top =
    Zipper.top >> Zipper.zTableau


topRenumbered =
    top >> Zipper.renumber


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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
    div [ class "tableau" ]
        [ div
            [ class "container" ]
            [ text "Tableau:"
            , viewNode (Zipper.zipper model.tableau)
            ]
        ]


viewNode : Zipper.Zipper -> Html Msg
viewNode z =
    let
        ( tableau, bs ) =
            z
    in
        div
            [ class "formula" ]
            --            text |< --> todo
            [ input [ size ((String.length tableau.node.value) * 3 // 4 + 1), value ((Zipper.zNode z).id |> toString) ] []
            , input [ class "formulaEdit ", value (Zipper.zNode z).value, type_ "text", onInput <| ChangeText z ] []
            , input
                [ class "refEdit "
                , value (Zipper.zNode z).reference.str
                , size ((String.length tableau.node.value) * 3 // 4 + 1)
                ]
                []
            , viewChildren z
            ]


viewChildren : Zipper.Zipper -> Html Msg
viewChildren z =
    case (Zipper.zTableau z).ext of
        Zipper.Open ->
            viewOpen z

        Zipper.Closed r1 r2 ->
            div [] [ text "*" ]

        Zipper.Alpha t ->
            div [] [ viewAlpha (Zipper.down z) ]

        --            div [] [ viewNode (Zipper.down z) ]
        Zipper.Beta lt rt ->
            div [] [ viewBeta z ]



--            div [] [ viewNode (Zipper.left z), viewNode (Zipper.right z) ]


viewOpen : Zipper.Zipper -> Html Msg
viewOpen z =
    div []
        [ text (Zipper.zNode z).value
        , expandControls z
        ]



--viewClosed


viewAlpha : Zipper.Zipper -> Html Msg
viewAlpha z =
    table []
        [ tr []
            [ td []
                [ viewNode z ]
            ]
        ]


viewBeta : Zipper.Zipper -> Html Msg
viewBeta z =
    table []
        [ tr []
            [ td [ class "beta" ]
                [ viewNode (Zipper.left z) ]
            , td [ class "beta" ]
                [ viewNode (Zipper.right z) ]
            ]
        ]


expandControls z =
    let
        ( t, bs ) =
            z
    in
        --        [ div [ class "expandControls" ]
        (case t.ext of
            Zipper.Open ->
                div [ class "expandControls" ]
                    [ button [ onClick (ExpandAlpha z) ] [ text "α" ]
                    , button [ onClick (ExpandBeta z) ] [ text "β" ]
                    , button [ onClick (MakeClosed z) ] [ text "*" ]
                    ]

            --                        Just ( r1, r2 ) ->
            --                            let
            --                                compl =
            --                                    Errors.errors <| areCloseRefsComplementary r1 r2 z
            --
            --                                ref1Cls =
            --                                    problemsClass <| (validateRef "Invalid close ref. #1" r1 z) ++ compl
            --
            --                                ref2Cls =
            --                                    problemsClass <| (validateRef "Invalid close ref. #1" r2 z) ++ compl
            --                            in
            --                                [ text "* "
            --                                , input
            --                                    [ class ("refEdit " ++ ref1Cls)
            --                                    , type_ "text"
            --                                    , placeholder "Ref"
            --                                    , size 1
            --                                    , value r1.str
            --                                    , onInput <| SetClosed 0 z
            --                                    ]
            --                                    []
            --                                , input
            --                                    [ class ("refEdit " ++ ref2Cls)
            --                                    , type_ "text"
            --                                    , placeholder "Ref"
            --                                    , size 1
            --                                    , value r2.str
            --                                    , onInput <| SetClosed 1 z
            --                                    ]
            --                                    []
            --                                , button [ class "delete", onClick (MakeOpen z) ] [ text "x" ]
            --                                ]
            Zipper.Alpha _ ->
                p [] []

            Zipper.Beta _ _ ->
                p [] []

            Zipper.Closed _ _ ->
                p [] []
        )



--        ]
