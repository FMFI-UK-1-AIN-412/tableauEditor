module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Parser


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


type alias Node =
    { id : Int, value : String, reference : Int }


type alias Model =
    { nodes : List Node
    }


init =
    ( { nodes = [ { id = 1, value = "", reference = 1 } ]
      }
    , Cmd.none
    )


type Msg
    = ChangeText String String
    | ExpandInput
    | ChangeRef String String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeText id new ->
            let
                nodes =
                    List.map
                        (\n ->
                            if (toString n.id) == id then
                                { n | value = new }
                            else
                                n
                        )
                        model.nodes
            in
                ( { model | nodes = nodes }, Cmd.none )

        ExpandInput ->
            let
                lastNodeList =
                    List.drop ((List.length model.nodes) - 1) model.nodes

                maybeLastNode =
                    List.head lastNodeList

                lastNodeId =
                    case maybeLastNode of
                        Just node ->
                            node.id

                        Nothing ->
                            2

                nodes =
                    List.append model.nodes
                        [ { id = lastNodeId + 1
                          , value = ""
                          , reference = 0
                          }
                        ]
            in
                ( { model | nodes = nodes }, Cmd.none )

        ChangeRef id newRef ->
            let
                nodes =
                    List.map
                        (\n ->
                            if (toString n.id) == id then
                                { n | reference = Result.withDefault 0 (String.toInt newRef) }
                            else
                                n
                        )
                        model.nodes
            in
                ( { model | nodes = nodes }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div [ class "container" ]
            (List.map
                (\n ->
                    div [ class "form-row" ]
                        [ div [ class "col-md-8 left" ]
                            [ p [ class "id" ] [ text (toString n.id) ]
                            , input [ class "form-control", type_ "text", onInput <| ChangeText (toString n.id) ] []
                            ]
                        , div [ class "col-md-2 right" ]
                            [ input [ class "form-control", type_ "text", onInput <| ChangeRef (toString n.id) ] []
                            ]
                        , p [ class "valueValue" ] [ text n.value ]
                        , p [ class "referenceValue" ] [ text (toString n.reference) ]
                        , button [ class "btn btn-primary" ] [ text "delete" ]
                        ]
                )
                model.nodes
            )
        , button [ type_ "button", onClick ExpandInput ] [ text "New Input" ]
        ]
