port module Editor exposing (main, top, topRenumbered)
--, FileReaderPortData, fileContentRead, fileSelected

import Browser
import Errors
import File exposing (File)
import File.Select as Select
import File.Download as Download
import FontAwesome exposing (icon, ellipsisHorizontal, exchangeAlt)
import Formula exposing (Formula(..))
import Formula.Signed exposing (Signed(..))
import Formula.Parser
import Helpers.Helper as Helper
import Json.Decode
import Helpers.Exporting.Json.Decode
import Helpers.Exporting.Json.Encode
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Helpers.Rules as Rules exposing (..)
import Tableau exposing (..)
import Task
import UndoList exposing (UndoList)
import Validate
import Zipper exposing (..)


main : Program (Maybe String) Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type JsonImport
    = None
    | InProgress String
    | ImportErr String


type alias Model =
    UndoList
        { tableau : Tableau
        , jsonImport: JsonImport
        }


init : Maybe String -> ( Model, Cmd msg )
init mts =
    let
        emptyT =
            { node =
                { id = 1
                , value = ""
                , reference = { str = "1", up = Just 0 }
                , formula = Formula.Parser.parseSigned ""
                , gui = defGUI
                }
            , ext = Open
            }

        initT =
            case mts of
                Nothing -> emptyT
                Just ts ->
                    case Helpers.Exporting.Json.Decode.decode ts of
                        Ok t ->
                            t

                        Err _ ->
                            emptyT
    in
        ( UndoList.fresh
            { tableau = initT
            , jsonImport = None
            }
        , Cmd.none
        )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = ChangeText Zipper.Zipper String
    | ChangeRef Zipper.Zipper String
    | ExpandAlpha Zipper.Zipper
    | ExpandBeta Zipper.Zipper
    | Delete Zipper.Zipper
    | DeleteMe Zipper.Zipper
    | MakeClosed Zipper.Zipper
    | SetClosed Int Zipper.Zipper String
    | MakeOpen Zipper.Zipper
    | ExpandGamma Zipper.Zipper
    | ExpandDelta Zipper.Zipper
    | ChangeVariable Zipper.Zipper String
    | ChangeTerm Zipper.Zipper String
    | SwitchBetas Zipper.Zipper
    | ChangeToAlpha Zipper.Zipper
    | ChangeToBeta Zipper.Zipper
    | ChangeToGamma Zipper.Zipper
    | ChangeToDelta Zipper.Zipper
    | ChangeButtonsAppearance Zipper.Zipper
    | Undo
    | Redo
    | Prettify
    | JsonSelect
    | JsonSelected File
    | JsonRead String
    | Export
    | Print
    | Cache


port print : () -> Cmd msg


port cache : String -> Cmd msg


top : Zipper.Zipper -> Tableau
top =
    Zipper.top >> Zipper.zTableau


topRenumbered : Zipper.Zipper -> Tableau
topRenumbered =
    top >> Zipper.renumber


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ present } as model) =
    case msg of
        JsonSelect ->
            ( model
            , Select.file [ "application/json" ] JsonSelected
            )

        JsonSelected file ->
            ( { model
                | present =
                    { present
                    | jsonImport = InProgress (File.name file)
                    }
                }
            , Task.perform JsonRead (File.toString file)
            )

        JsonRead contents ->
            case contents |> Helpers.Exporting.Json.Decode.decode of
                Ok t ->
                    ( UndoList.new
                        { present | jsonImport = None, tableau = t }
                        model
                    , cache contents
                    )

                Err e ->
                    ( { model
                        | present =
                            { present
                            | jsonImport =
                                ImportErr (Json.Decode.errorToString e)
                            }
                        }
                    , Cmd.none
                    )

        Export ->
            ( model
            , Download.string
                "tableau.json"
                "application/json"
                <| Helpers.Exporting.Json.Encode.encode 2 present.tableau
            )

        Undo ->
            ( UndoList.undo
                { model | present = { present | jsonImport = None } }
            , Cmd.none
            )

        Redo ->
            ( UndoList.redo model, Cmd.none )

        Print ->
            ( model, print () )

        Cache ->
            ( model
            , cache (Helpers.Exporting.Json.Encode.encode 0 model.present.tableau) )

        _ ->
            let
                presentSansImport = { present | jsonImport = None }
            in
            ( UndoList.new
                (simpleUpdate msg presentSansImport)
                { model | present = presentSansImport }
            , Cmd.none )


simpleUpdate msg model =
    Debug.log "model"
        (case msg of
            ChangeText z new ->
                { model | tableau = z |> Zipper.setFormula new |> top }

            ExpandAlpha z ->
                { model | tableau = z |> Zipper.extendAlpha |> renumberJustInReferences Zipper.renumberJustInRefWhenExpanding |> topRenumbered }

            ExpandBeta z ->
                { model | tableau = z |> Zipper.extendBeta |> renumberJustInReferences Zipper.renumberJustInRefWhenExpanding |> topRenumbered }

            ExpandGamma z ->
                { model | tableau = z |> Zipper.extendGamma |> renumberJustInReferences Zipper.renumberJustInRefWhenExpanding |> topRenumbered }

            ExpandDelta z ->
                { model | tableau = z |> Zipper.extendDelta |> renumberJustInReferences Zipper.renumberJustInRefWhenExpanding |> topRenumbered }

            ChangeRef z new ->
                { model | tableau = z |> Zipper.setRef new |> top }

            Delete z ->
                { model | tableau = z |> Zipper.delete |> topRenumbered }

            DeleteMe z ->
                let
                    newZipp =
                        z |> Zipper.deleteMe
                in
                if newZipp /= (z |> up) then
                    { model | tableau = z |> Zipper.deleteMe |> renumberJustInReferences Zipper.renumberJustInRefWhenDeleting |> topRenumbered }

                else
                    { model | tableau = z |> Zipper.deleteMe |> topRenumbered }

            MakeClosed z ->
                { model | tableau = z |> Zipper.makeClosed |> top }

            SetClosed which z ref ->
                { model | tableau = z |> Zipper.setClosed which ref |> top }

            MakeOpen z ->
                { model | tableau = z |> Zipper.makeOpen |> top }

            ChangeVariable z newVariable ->
                { model | tableau = z |> Zipper.changeVariable newVariable |> top }

            ChangeTerm z newTerm ->
                { model | tableau = z |> Zipper.changeTerm newTerm |> top }

            SwitchBetas z ->
                { model | tableau = z |> Zipper.switchBetas |> topRenumbered }

            ChangeToAlpha z ->
                { model | tableau = z |> Zipper.changeToAlpha |> topRenumbered }

            ChangeToBeta z ->
                { model | tableau = z |> Zipper.changeToBeta |> topRenumbered }

            ChangeToGamma z ->
                { model | tableau = z |> Zipper.changeToGamma |> topRenumbered }

            ChangeToDelta z ->
                { model | tableau = z |> Zipper.changeToDelta |> topRenumbered }

            ChangeButtonsAppearance z ->
                { model | tableau = z |> Zipper.changeButtonAppearance |> top }

            Prettify ->
                { model | tableau = Zipper.prettify model.tableau }

            JsonSelect ->
                model

            JsonSelected _ ->
                model

            Undo ->
                model

            Redo ->
                model

            JsonRead _ ->
                model

            Export ->
                model

            Print ->
                model

            Cache ->
                model
        )


view : Model -> Browser.Document Msg
view ({ present } as model) =
    { title = "Tableau Editor"
    , body =
        [   div [ class "tableau" ]
            [ div [ class "actions" ]
                [ button [ class "button", onClick Prettify ] [ text "Prettify formulas" ]
                , button [ class "button", onClick Print ] [ text "Print" ]
                , jsonExportControl present.tableau
                , jsonImportControl present.jsonImport
                , button [ class "button", onClick Undo ] [ text "Undo" ]
                , button [ class "button", onClick Redo ] [ text "Redo" ]
                ]
            , jsonImportError present.jsonImport
            , viewNode (Zipper.zipper present.tableau)
            , verdict present.tableau
            , problems present.tableau
            , Rules.help
            ]
        ]
    }


viewNode : Zipper.Zipper -> Html Msg
viewNode z =
    div
        [ class "formula" ]
        [ viewNodeInputs identity z
        , singleNodeProblems z
        , viewControls z
        , viewChildren z
        ]


viewSubsNode : Zipper.Zipper -> Html Msg
viewSubsNode z =
    div [ class "formula" ]
        [ viewNodeInputs
            (\rest -> text "{"
            :: autoSizeInput
                (z |> up |> Zipper.zSubstitution |> Maybe.map .var |> Maybe.withDefault "")
                [ classList
                    [ ( "textInput textInputVariable", True )
                    , ( "semanticsProblem", Helper.hasReference z )
                    ]
                , onInput <| ChangeVariable z
                ]
            :: text "→"
            :: autoSizeInput
                (z |> up |> Zipper.zSubstitution |> Maybe.map .term |> Maybe.withDefault "")
                [ classList
                    [ ( "textInput textInputTerm", True )
                    , ( "semanticsProblem", Helper.hasReference z )
                    ]
                , onInput <| ChangeTerm z
                ]
            :: text "}"
            :: rest
            )
            z
        , singleNodeProblems z
        , viewControls z
        , viewChildren z
        ]


viewNodeInputs : (List (Html Msg) -> List (Html Msg)) -> Zipper.Zipper
    -> Html Msg
viewNodeInputs additional z =
    div [ class "inputGroup" ]
            ( text ("(" ++ ((Zipper.zNode z).id |> String.fromInt) ++ ")")
            :: autoSizeInput
                (Zipper.zNode z).value
                [ classList
                    [ ( "textInputFormula", True )
                    , ( "premise", Helper.isPremise z )
                    ]
                , class (errorsClass <| Validate.isCorrectFormula z)
                , type_ "text"
                , onInput <| ChangeText z
                ]
            :: viewRuleType z
            :: div [ class "onclick-menu change", tabindex 0 ]
                [ ul [ class "onclick-menu-content" ]
                    [ li [] [ button [ onClick (ChangeToAlpha z) ] [ text "Change to α" ] ]
                    , li [] [ button [ onClick (ChangeToBeta z) ] [ text "Change to β" ] ]
                    , li [] [ button [ onClick (ChangeToGamma z) ] [ text "Change to γ" ] ]
                    , li [] [ button [ onClick (ChangeToDelta z) ] [ text "Change to δ" ] ]
                    ]
                ]
            :: text "["
            :: autoSizeInput
                (Zipper.zNode z).reference.str
                [ class "textInputReference"
                , onInput <| ChangeRef z
                , class (problemsClass <| Validate.validateNodeRef z)
                ]
            :: text "]"
            :: additional
                [ viewButtonsAppearanceControlls z ]
            )


autoSizeInput : String -> List (Attribute Msg) -> Html Msg
autoSizeInput val attrs =
    input
        ( type_ "text"
        :: class "textInput"
        :: value val
        -- :: size (String.length val + 1)
        :: size ((String.length val * 5 + 9) // 6)
        :: onBlur Cache
        :: attrs
        )
        []


viewRuleType : Zipper.Zipper -> Html Msg
viewRuleType z =
    if Helper.isPremise z then
        span [] [ var [] [ text "S" ], sup [] [ text "+" ] ]
    else
        case (Zipper.zTableau <| Zipper.up z).ext of
            Open ->
                text "O"
            Closed _ _ ->
                text "C"
            Alpha _ ->
                text "α"
            Beta _ _ ->
                text "β"
            Gamma _ _ ->
                text "γ"
            Delta _ _ ->
                text "δ"


viewButtonsAppearanceControlls : Zipper.Zipper -> Html Msg
viewButtonsAppearanceControlls z =
    case (Zipper.zTableau z).ext of
        Closed _ _ ->
            div [] []

        _ ->
            button
                [ class "button"
                , classList
                    [ ( "active"
                      , (Zipper.zTableau z).node.gui.controlsShown )
                    ]
                , onClick (ChangeButtonsAppearance z)
                , title "Toggle node tools"
                ]
                [ icon ellipsisHorizontal ]


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
    div [] []


viewClosed : Zipper.Zipper -> Html Msg
viewClosed z =
    div [] []


viewControls : Zipper.Zipper -> Html Msg
viewControls ( ( t, _ )  as z ) =
    div [ class "expandControls" ]
        (case t.ext of
            Tableau.Closed r1 r2 ->
                let
                    compl =
                        Errors.errors <| Validate.areCloseRefsComplementary r1 r2 z

                    ref1Cls =
                        problemsClass <| Validate.validateRef "Invalid close ref. #1" r1 z ++ compl

                    ref2Cls =
                        problemsClass <| Validate.validateRef "Invalid close ref. #2" r2 z ++ compl
                in
                [ text "* "
                , autoSizeInput r1.str
                    [ class ("closed " ++ ref1Cls)
                    , placeholder "Ref"
                    , onInput <| SetClosed 0 z
                    ]
                , text " "
                , autoSizeInput r2.str
                    [ class ("closed " ++ ref2Cls)
                    , placeholder "Ref"
                    , onInput <| SetClosed 1 z
                    ]
                , button [ class "button", onClick (MakeOpen z) ] [ text "Open" ]
                ]

            _ ->
                let
                    deleteMeButton =
                        if (z |> Zipper.up) /= z then
                            case z |> Zipper.up |> Zipper.zTableau |> .ext of
                                Beta _ _ ->
                                    case t.node.value of
                                        "" ->
                                            case t.ext of
                                                Open ->
                                                    button [ onClick (DeleteMe z) ] [ text "Delete node" ]

                                                _ ->
                                                    div [] []

                                        _ ->
                                            div [] []

                                _ ->
                                    button [ onClick (DeleteMe z) ] [ text "Delete node" ]
                        else
                            case t.ext of
                                Alpha _ ->
                                    button [ onClick (DeleteMe z) ] [ text "Delete node" ]

                                Open ->
                                    button [ onClick (DeleteMe z) ] [ text "Delete node" ]

                                _ ->
                                    div [] []

                    switchBetasButton =
                        case t.ext of
                            Beta _ _ ->
                                button [ class "button", onClick (SwitchBetas z), title "Swap branches" ] [ icon exchangeAlt ]

                            _ ->
                                div [] []
                in
                if t.node.gui.controlsShown then
                    [ button [ class "button", onClick (ExpandAlpha z) ] [ text "Add α" ]
                    , div [ class "onclick-menu add", tabindex 0 ]
                        [ ul [ class "onclick-menu-content" ]
                            [ li [] [ button [ onClick (ExpandAlpha z) ] [ text "Add α" ] ]
                            , li [] [ button [ onClick (ExpandBeta z) ] [ text "Add β" ] ]
                            , li [] [ button [ onClick (ExpandGamma z) ] [ text "Add γ" ] ]
                            , li [] [ button [ onClick (ExpandDelta z) ] [ text "Add δ" ] ]
                            ]
                        ]
                    , div [ class "onclick-menu del", tabindex 0 ]
                        [ ul [ class "onclick-menu-content" ]
                            [ li [] [ deleteMeButton ]
                            , li [] [ button [ onClick (Delete z) ] [ text "Delete subtree" ] ]
                            ]
                        ]
                    , button [ class "button", onClick (MakeClosed z) ] [ text "Close" ]
                    , switchBetasButton
                    ]
                else
                    []
        )


singleNodeProblems : Zipper -> Html Msg
singleNodeProblems z =
    let
        errors =
            Errors.errors <| Validate.isCorrectNode <| z
    in
    if List.isEmpty errors then
        div [ class "nodeProblems" ] []

    else
        div [ class "nodeProblems" ]
            (List.map
                (\pr -> small [ class "nodeProblemsText" ] [ text <| pr.msg ])
                errors
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
        , text <| String.fromInt <| .id <| Zipper.zNode <| pi.zip
        , text ") "
        , text <| pi.msg
        ]


errorsClass : Result (List Validate.Problem) a -> String
errorsClass =
    Errors.errors >> problemsClass


problemsClass : List Validate.Problem -> String
problemsClass pl =
    case pl of
        [] ->
            ""

        p :: _ ->
            problemClass p


problemClass : { a | typ : Validate.ProblemType } -> String
problemClass { typ } =
    case typ of
        Validate.Syntax ->
            "syntaxProblem"

        Validate.Semantics ->
            "semanticsProblem"


jsonExportControl : Tableau -> Html Msg
jsonExportControl t =
    button [ class "button", onClick Export ] [ text "Export as JSON" ]


jsonImportControl : JsonImport -> Html Msg
jsonImportControl jsonImport =
    case jsonImport of
        InProgress fname ->
            text <| "Loading tableau from file" ++ fname ++ "…"

        _ ->
            button
                [ class "button", onClick JsonSelect ]
                [ text "Import from JSON" ]


jsonImportError : JsonImport -> Html msg
jsonImportError jsonImport =
    case jsonImport of
        ImportErr e ->
            p
                [ class "jsonImportError" ]
                [ text <| "Error importing tableau: " ++ e ]

        _ ->
            div [] []


verdict : Tableau -> Html msg
verdict t =
    let
        ass =
            t |> Zipper.zipper |> Helper.assumptions

        ( premises, conclusions ) =
            List.partition
                (\sf ->
                    case sf of
                        T _ ->
                            True

                        F _ ->
                            False
                )
                ass
    in
    if List.isEmpty ass then
        div [ class "verdict" ] [ p [] [ text "This tableau doesn't prove anything." ] ]

    else
        div [ class "verdict" ]
            [ p []
                [ text "This tableau "
                , text (textVerdict <| Zipper.zipper t)
                , text ":"
                ]
            , p []
                [ text (premises |> List.map (Formula.Signed.getFormula >> Formula.toString) |> String.join " , ")
                , text " ⊦ "
                , text (conclusions |> List.map (Formula.Signed.getFormula >> Formula.toString) |> String.join " , ")
                ]
            ]


textVerdict : Zipper -> String
textVerdict t =
    case Helper.isClosed t of
        Ok True ->
            "proves"

        Ok False ->
            "does not prove"

        Err _ ->
            "might be proving (once correct)"
