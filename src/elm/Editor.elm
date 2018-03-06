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
import Validate
import Dict


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
    | SetClosed Int Zipper.Zipper String
    | MakeOpen Zipper.Zipper
    | ExpandGamma Zipper.Zipper
    | ExpandDelta Zipper.Zipper
    | ChangeVariable Zipper.Zipper String
    | ChangeTerm Zipper.Zipper String


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

            ExpandGamma z ->
                ( { model | tableau = (z |> Zipper.extendGamma |> topRenumbered) }, Cmd.none )

            ExpandDelta z ->
                ( { model | tableau = (z |> Zipper.extendDelta |> topRenumbered) }, Cmd.none )

            ChangeRef z new ->
                ( { model | tableau = (z |> Zipper.setRef new |> top) }, Cmd.none )

            Delete z ->
                ( { model | tableau = (z |> Zipper.delete |> topRenumbered) }, Cmd.none )

            MakeClosed z ->
                ( { model | tableau = (z |> Zipper.makeClosed |> top) }, Cmd.none )

            SetClosed which z ref ->
                ( { model | tableau = (z |> Zipper.setClosed which ref |> top) }, Cmd.none )

            MakeOpen z ->
                ( { model | tableau = (z |> Zipper.makeOpen |> top) }, Cmd.none )

            ChangeVariable z newVariable ->
                ( { model | tableau = ((Zipper.up z) |> (Zipper.changeVariable newVariable) |> top) }, Cmd.none )

            ChangeTerm z newTerm ->
                ( { model | tableau = ((Zipper.up z) |> (Zipper.changeTerm newTerm) |> top) }, Cmd.none )
        )


view : Model -> Html Msg
view model =
    div [ class "tableau" ]
        [ viewNode (Zipper.zipper model.tableau)
        , problems model.tableau
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


viewSubsNode : Zipper.Zipper -> Html Msg
viewSubsNode z =
    let
        ( tableau, bs ) =
            z
    in
        div
            [ class "formula" ]
            [ text <| "(" ++ ((Zipper.zNode z).id |> toString) ++ ")"
            , input
                [ classList
                    [ ( "formulaInputSubst", True )
                    , ( "semanticsProblem", Helper.hasReference z )
                    ]
                , value (Zipper.zNode z).value
                , type_ "text"
                , onInput <| ChangeText z
                ]
                []
            , text "Substituting"
            , input
                [ classList
                    [ ( "substitutedVariable", True )
                    , ( "semanticsProblem", Helper.hasReference z )
                    ]
                , value (z |> up |> Zipper.zSubstitution |> Maybe.map .what |> Maybe.withDefault "")
                , type_ "text"
                , onInput <| ChangeVariable z
                ]
                []
            , text "for"
            , input
                [ classList
                    [ ( "substitutedConstant", True )
                    , ( "semanticsProblem", Helper.hasReference z )
                    ]
                , value (z |> up |> Zipper.zSubstitution |> Maybe.map .forWhat |> Maybe.withDefault "")
                , type_ "text"
                , onInput <| ChangeTerm z
                ]
                []
            , text "["
            , input
                [ class "formulaReference"
                , value (Zipper.zNode z).reference.str
                , onInput <| ChangeRef z
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
            viewClosed z

        Tableau.Alpha t ->
            viewAlpha z

        Tableau.Beta lt rt ->
            viewBeta z

        Tableau.Gamma t subs ->
            viewGamma z

        Tableau.Delta t subs ->
            viewDelta z


viewAlpha : Zipper.Zipper -> Html Msg
viewAlpha z =
    div [ class "alpha" ] [ viewNode (Zipper.down z) ]


viewBeta : Zipper.Zipper -> Html Msg
viewBeta z =
    div [ class "beta" ]
        [ viewNode (Zipper.left z)
        , viewNode (Zipper.right z)
        ]


viewGamma : Zipper.Zipper -> Html Msg
viewGamma z =
    div [ class "gamma" ] [ viewSubsNode (Zipper.down z) ]


viewDelta : Zipper.Zipper -> Html Msg
viewDelta z =
    div [ class "delta" ] [ viewSubsNode (Zipper.down z) ]


viewOpen : Zipper.Zipper -> Html Msg
viewOpen z =
    div [ class "open" ]
        [ viewControls z
        ]


viewClosed : Zipper.Zipper -> Html Msg
viewClosed z =
    div [ class "open" ] [ viewControls z ]


viewControls : Zipper.Zipper -> Html Msg
viewControls z =
    let
        ( t, bs ) =
            z
    in
        div [ class "expandControls" ]
            (case t.ext of
                Tableau.Open ->
                    [ button [ onClick (ExpandAlpha z) ] [ text "α" ]
                    , button [ onClick (ExpandBeta z) ] [ text "β" ]
                    , button [ onClick (ExpandGamma z) ] [ text "γ" ]
                    , button [ onClick (ExpandDelta z) ] [ text "δ" ]
                    , button [ class "delete", onClick (MakeClosed z) ] [ text "*" ]
                    ]

                Tableau.Alpha _ ->
                    []

                Tableau.Gamma _ _ ->
                    []

                Tableau.Delta _ _ ->
                    []

                Tableau.Beta _ _ ->
                    []

                Tableau.Closed r1 r2 ->
                    let
                        compl =
                            Errors.errors <| Validate.areCloseRefsComplementary r1 r2 z

                        ref1Cls =
                            problemsClass <| (Validate.validateRef "Invalid close ref. #1" r1 z) ++ compl

                        ref2Cls =
                            problemsClass <| (Validate.validateRef "Invalid close ref. #1" r2 z) ++ compl
                    in
                        [ text "* "
                        , input
                            [ class ("refEdit " ++ ref1Cls)
                            , type_ "text"
                            , placeholder "Ref"
                            , size 1
                            , value r1.str
                            , onInput <| SetClosed 0 z
                            ]
                            []
                        , input
                            [ class ("refEdit " ++ ref2Cls)
                            , type_ "text"
                            , placeholder "Ref"
                            , size 1
                            , value r2.str
                            , onInput <| SetClosed 1 z
                            ]
                            []
                        , button [ class "delete", onClick (MakeOpen z) ] [ text "x" ]
                        ]
            )


problems : Tableau -> Html Msg
problems t =
    let
        errors =
            Errors.errors <| Validate.isCorrectTableau <| Zipper.zipper <| t
    in
        if List.isEmpty errors then
            div [ class "problems" ] []
        else
            div [ class "problems" ]
                [ p [] [ text "Problems" ]
                , problemList <| errors
                ]


problemList : List Validate.Problem -> Html Msg
problemList pl =
    ul [ class "problemList" ] (List.map problemItem pl)


problemItem : Validate.Problem -> Html Msg
problemItem pi =
    li [ class (problemClass pi) ]
        [ text "("
        , text <| toString <| .id <| Zipper.zNode <| pi.zip
        , text ") "
        , text <| pi.msg
        ]


problemsClass : List Validate.Problem -> String
problemsClass pl =
    case pl of
        [] ->
            ""

        p :: _ ->
            problemClass p


problemClass { typ } =
    case typ of
        Validate.Syntax ->
            "syntaxProblem"

        Validate.Semantics ->
            "semanticsProblem"
