port module Editor exposing (main, top, topRenumbered)

--, FileReaderPortData, fileContentRead, fileSelected

import Browser
import Config exposing (Config)
import Dict exposing (Dict)
import Errors
import File exposing (File)
import File.Download as Download
import File.Select as Select
import FontAwesome exposing (ellipsisHorizontal, exchangeAlt, icon)
import Formula exposing (Formula(..))
import Formula.Parser
import Formula.Signed exposing (Signed(..))
import Helpers.Exporting.Json.Decode
import Helpers.Exporting.Json.Encode
import Helpers.Helper as Helper
import Helpers.Rules as Rules exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Json.Encode exposing (set)
import Set
import Tableau exposing (..)
import Task
import UndoList exposing (UndoList)
import Validation
import Validation.Common exposing (Problem, ProblemType(..))
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
    | ImportErr String (List Json.Decode.Error)


type alias Model =
    UndoList
        { tableau : Tableau
        , jsonImport : JsonImport
        , config : Config
        }


init : Maybe String -> ( Model, Cmd msg )
init mts =
    let
        emptyT =
            { node =
                { id = 1
                , value = ""
                , references = []
                , formula = Formula.Parser.parseSigned ""
                , gui = defGUI
                }
            , ext = Open
            }

        (initCfg, initT) =
            case mts of
                Nothing ->
                    (Config.default, emptyT)

                Just ts ->
                    Helpers.Exporting.Json.Decode.decode ts 
                    |> (\(cfg, t) -> 
                        (cfg |> Result.withDefault Config.default, t |> Result.withDefault emptyT))
    in
    ( UndoList.fresh
        { tableau = initT
        , jsonImport = None
        , config = initCfg
        }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = ChangeText Zipper.Zipper String
    | ChangeRef Zipper.Zipper String
    | Delete Zipper.Zipper
    | DeleteMe Zipper.Zipper
    | MakeClosed Zipper.Zipper
    | SetClosed Int Zipper.Zipper String
    | MakeOpen Zipper.Zipper
    | ExpandUnary Tableau.UnaryExtType Zipper.Zipper
    | ExpandUnaryWithSubst Tableau.UnaryWithSubstExtType Zipper.Zipper
    | ExpandBinary Tableau.BinaryExtType Zipper.Zipper
    | ChangeSubst Zipper.Zipper String
    | SwitchBetas Zipper.Zipper
    | ChangeToUnary Tableau.UnaryExtType Zipper.Zipper
    | ChangeToUnaryWithSubst Tableau.UnaryWithSubstExtType Zipper.Zipper
    | ChangeToBinary Tableau.BinaryExtType Zipper.Zipper
    | ChangeButtonsAppearance Zipper.Zipper
    | SetConfig Config.Config
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
                ( Ok cfg, Ok t ) ->
                    ( UndoList.new
                        { present | jsonImport = None, config = cfg, tableau = t }
                        model
                    , cache contents
                    )

                ( Err cfgErr, Ok t ) ->
                    ( UndoList.new
                        { present
                        | jsonImport =
                            ImportErr
                                ("Failed to import rule set configuration. "
                                    ++ "Keeping the last one.")
                                [ cfgErr ]
                        , tableau = t
                        }
                        model
                    , cache contents
                    )

                ( Ok _, Err tErr ) ->
                    ( { model
                        | present =
                            { present
                            | jsonImport =
                                ImportErr
                                    "Failed to import tableau"
                                    [ tErr ]
                            }
                      }
                    , Cmd.none
                    )

                ( Err cfgErr, Err tErr ) ->
                    ( { model
                        | present =
                            { present
                            | jsonImport =
                                ImportErr
                                    ("Failed to import tableau and " ++
                                        "rule set configuration")
                                    [ tErr, cfgErr ]
                            }
                      }
                    , Cmd.none
                    )

        Export ->
            ( model
            , Download.string
                "tableau.json"
                "application/json"
              <|
                Helpers.Exporting.Json.Encode.encode 2 present.config present.tableau
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
            , cache (Helpers.Exporting.Json.Encode.encode 0 model.present.config model.present.tableau)
            )

        _ ->
            let
                presentSansImport =
                    { present | jsonImport = None }
            in
            ( UndoList.new
                (simpleUpdate msg presentSansImport)
                { model | present = presentSansImport }
            , Cmd.none
            )


simpleUpdate msg model =
    Debug.log "model"
        (case msg of
            ChangeText z new ->
                { model | tableau = z |> Zipper.setFormula new |> top }

            ExpandUnary extType z ->
                { model | tableau = z |> Zipper.extendUnary extType |> renumberJustInReferences Zipper.renumberJustInRefWhenExpanding |> topRenumbered }

            ExpandUnaryWithSubst extType z ->
                { model | tableau = z |> Zipper.extendUnaryWithSubst extType |> renumberJustInReferences Zipper.renumberJustInRefWhenExpanding |> topRenumbered }

            ExpandBinary extType z ->
                { model | tableau = z |> Zipper.extendBinary extType |> renumberJustInReferences Zipper.renumberJustInRefWhenExpanding |> topRenumbered }

            ChangeRef z new ->
                { model | tableau = z |> Zipper.setRefs new |> top }

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

            ChangeSubst z newSubst ->
                { model | tableau = z |> Zipper.setSubstitution newSubst |> top }

            SwitchBetas z ->
                { model | tableau = z |> Zipper.switchBetas |> topRenumbered }

            ChangeToUnary extType z ->
                { model | tableau = z |> Zipper.changeToUnaryRule extType |> topRenumbered }

            ChangeToUnaryWithSubst extType z ->
                { model | tableau = z |> Zipper.changeToUnaryRuleWithSubst extType |> topRenumbered }

            ChangeToBinary extType z ->
                { model | tableau = z |> Zipper.changeToBinaryRule extType |> topRenumbered }

            ChangeButtonsAppearance z ->
                { model | tableau = z |> Zipper.changeButtonAppearance |> top }

            SetConfig new ->
                { model | config = new }

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
        [ div [ class "tableau" ]
            [ div [ class "actions" ]
                [ configMenu present.config
                , button [ class "button", onClick Prettify ] [ text "Prettify formulas" ]
                , button [ class "button", onClick Print ] [ text "Print" ]
                , jsonExportControl present.tableau
                , jsonImportControl present.jsonImport
                , button [ class "button", onClick Undo ] [ text "Undo" ]
                , button [ class "button", onClick Redo ] [ text "Redo" ]
                ]
            , jsonImportError present.jsonImport
            , viewNode present.config (Zipper.zipper present.tableau)
            , verdict present.config present.tableau
            , problems present.config present.tableau
            , Rules.help present.config
            ]
        ]
    }


viewNode : Config -> Zipper.Zipper -> Html Msg
viewNode config z =
    div
        [ class "formula" ]
        [ viewNodeInputs identity config z
        , singleNodeProblems config z
        , viewControls config z
        , viewChildren config z
        ]


viewSubsNode : Config -> Zipper.Zipper -> Html Msg
viewSubsNode config z =
    div [ class "formula" ]
        [ viewNodeInputs
            (\rest ->
                text "{"
                    :: autoSizeInput
                        (z |> up |> Zipper.zSubstitution |> Maybe.map .str |> Maybe.withDefault "")
                        [ classList
                            [ ( "textInput textInputSubst", True )
                            , ( "semanticsProblem", Helper.hasReference z )
                            ]
                        , onInput <| ChangeSubst z
                        , placeholder "a↦b,.."
                        ]
                    :: text "}"
                    :: rest
            )
            config
            z
        , singleNodeProblems config z
        , viewControls config z
        , viewChildren config z
        ]


viewNodeInputs :
    (List (Html Msg) -> List (Html Msg))
    -> Config
    -> Zipper.Zipper
    -> Html Msg
viewNodeInputs additional config z =
    div [ class "inputGroup" ]
        (text ("(" ++ ((Zipper.zNode z).id |> String.fromInt) ++ ")")
            :: autoSizeInput
                (Zipper.zNode z).value
                [ classList
                    [ ( "textInputFormula", True )
                    , ( "premise", Helper.isPremise z )
                    ]
                , class (errorsClass <| Validation.isCorrectFormula config z)
                , type_ "text"
                , onInput <| ChangeText z
                ]
            :: ruleMenu ChangeToUnary ChangeToUnaryWithSubst ChangeToBinary (viewRuleType z) "Change to" "change" config z
            :: text "["
            :: autoSizeInput
                (Tableau.refsToString (Zipper.zNode z).references)
                [ class "textInputReference"
                , onInput <| ChangeRef z
                , class (problemsClass <| Validation.validateNodeRef z)
                ]
            :: text "]"
            :: additional
                [ viewButtonsAppearanceControlls z ]
        )


configMenu config =
    let
        item cfg =
            menuItem (SetConfig cfg) (Config.toString cfg)
    in
    menu "change" (text <| Config.toString config) <|
        [ item Config.BasicPropositional
        , item Config.Propositional
        , item Config.PropositionalWithEquality
        , item Config.BasicFol
        , item Config.FullFol
        ]


ruleMenu unaryMsg unaryWithSubstMsg binaryMsg label labelPrefix cls config z =
    let
        item ruleTypeStr msg =
            if Set.member ruleTypeStr <| Config.getRuleSet config then
                Just <| menuItem msg (labelPrefix ++ " " ++ ruleTypeStr)

            else
                Nothing

        unaryItem extType =
            item (unaryExtTypeToString extType) (unaryMsg extType z)

        unaryWithSubstItem extType =
            item (unaryWithSubstExtTypeToString extType) (unaryWithSubstMsg extType z)

        binaryItem extType =
            item (binaryExtTypeToString extType) (binaryMsg extType z)
    in
    menu cls label <|
        List.filterMap unaryItem [ Alpha ]
            ++ List.filterMap binaryItem [ Beta ]
            ++ List.filterMap unaryWithSubstItem [ Gamma, Delta, GammaStar, DeltaStar ]
            ++ List.filterMap unaryItem [ Refl, Leibnitz, MP, MT, HS, DS, NCS, ESFF, ESFT, ESTF, ESTT ]
            ++ List.filterMap binaryItem [ Cut, ECDF, ECDT ]


menuItem : Msg -> String -> Html Msg
menuItem msg str =
    li [] [ button [ onClick msg ] [ text str ] ]


menu cls label content =
    div [ class <| "onclick-menu " ++ cls, tabindex 0 ]
        [ span [] [ label, text " ▾" ]
        , ul [ class "onclick-menu-content" ] content
        ]


autoSizeInput : String -> List (Attribute Msg) -> Html Msg
autoSizeInput val attrs =
    input
        (type_ "text"
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

            Unary extType _ ->
                text (unaryExtTypeToString extType)

            Binary extType _ _ ->
                text (binaryExtTypeToString extType)

            UnaryWithSubst extType _ _ ->
                text (unaryWithSubstExtTypeToString extType)


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
                      , (Zipper.zTableau z).node.gui.controlsShown
                      )
                    ]
                , onClick (ChangeButtonsAppearance z)
                , title "Toggle node tools"
                ]
                [ icon ellipsisHorizontal ]


viewChildren : Config -> Zipper.Zipper -> Html Msg
viewChildren config z =
    case (Zipper.zTableau z).ext of
        Open ->
            viewOpen z

        Closed r1 r2 ->
            viewClosed z

        Unary _ _ ->
            viewUnary config z

        UnaryWithSubst _ _ _ ->
            viewUnaryWithSubst config z

        Binary _ _ _ ->
            viewBinary config z


viewUnary : Config -> Zipper.Zipper -> Html Msg
viewUnary config z =
    div [ class "alpha" ] [ viewNode config (Zipper.down z) ]


viewUnaryWithSubst : Config -> Zipper.Zipper -> Html Msg
viewUnaryWithSubst config z =
    div [ class "alpha", class "withSubstitution" ] [ viewSubsNode config (Zipper.down z) ]


viewBinary : Config -> Zipper.Zipper -> Html Msg
viewBinary config z =
    div [ class "beta" ]
        [ viewNode config (Zipper.left z)
        , viewNode config (Zipper.right z)
        ]


viewOpen : Zipper.Zipper -> Html Msg
viewOpen z =
    div [] []


viewClosed : Zipper.Zipper -> Html Msg
viewClosed z =
    div [] []


viewControls : Config -> Zipper.Zipper -> Html Msg
viewControls config (( t, _ ) as z) =
    div [ class "expandControls" ]
        (case t.ext of
            Tableau.Closed r1 r2 ->
                let
                    compl =
                        Errors.errors <| Validation.areCloseRefsComplementary r1 r2 z

                    ref1Cls =
                        problemsClass <| Validation.validateRef "Invalid close ref. #1" r1 z ++ compl

                    ref2Cls =
                        problemsClass <| Validation.validateRef "Invalid close ref. #2" r2 z ++ compl
                in
                [ text "* "
                , autoSizeInput r1.str
                    [ class ("closed " ++ ref1Cls)
                    , placeholder "Ref"
                    , onInput <| SetClosed 0 z
                    ]
                , text "\u{00A0}"
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
                                Binary _ _ _ ->
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
                                Unary Alpha _ ->
                                    button [ onClick (DeleteMe z) ] [ text "Delete node" ]

                                Open ->
                                    button [ onClick (DeleteMe z) ] [ text "Delete node" ]

                                _ ->
                                    div [] []

                    switchBetasButton =
                        case t.ext of
                            Binary _ _ _ ->
                                button [ class "button", onClick (SwitchBetas z), title "Swap branches" ] [ icon exchangeAlt ]

                            _ ->
                                div [] []
                in
                if t.node.gui.controlsShown then
                    [ button [ class "button", onClick (ExpandUnary Alpha z) ] [ text "Add α" ]
                    , ruleMenu ExpandUnary ExpandUnaryWithSubst ExpandBinary (text "Add") "Add" "add" config z
                    , menu "del" (text "Delete") <|
                        [ li [] [ deleteMeButton ]
                        , li [] [ button [ onClick (Delete z) ] [ text "Delete subtree" ] ]
                        ]
                    , button [ class "button", onClick (MakeClosed z) ] [ text "Close" ]
                    , switchBetasButton
                    ]

                else
                    []
        )


singleNodeProblems : Config -> Zipper -> Html Msg
singleNodeProblems config z =
    let
        errors =
            Errors.errors <| Validation.isCorrectNode config <| z
    in
    if List.isEmpty errors then
        div [ class "nodeProblems" ] []

    else
        div [ class "nodeProblems" ]
            (List.map
                (\pr -> small [ class "nodeProblemsText" ] [ text <| pr.msg ])
                errors
            )


problems : Config -> Tableau -> Html Msg
problems config t =
    let
        errors =
            Errors.errors <| Validation.isCorrectTableau config <| Zipper.zipper <| t
    in
    if List.isEmpty errors then
        div [ class "problems" ] []

    else
        div [ class "problems" ]
            [ p [] [ text "Problems" ]
            , problemList <| errors
            ]


problemList : List Problem -> Html Msg
problemList pl =
    ul [ class "problemList" ] (List.map problemItem pl)


problemItem : Problem -> Html Msg
problemItem pi =
    li [ class (problemClass pi) ]
        [ text "("
        , text <| String.fromInt <| .id <| Zipper.zNode <| pi.zip
        , text ") "
        , text <| pi.msg
        ]


errorsClass : Result (List Problem) a -> String
errorsClass =
    Errors.errors >> problemsClass


problemsClass : List Problem -> String
problemsClass pl =
    case pl of
        [] ->
            ""

        p :: _ ->
            problemClass p


problemClass : Problem -> String
problemClass { typ } =
    case typ of
        Syntax ->
            "syntaxProblem"

        Semantics ->
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
        ImportErr msg ds ->
            div [ class "jsonImportError" ]
                <| List.singleton <| div []
                    [ p [] [text <| "Import error: " ++ msg ]
                    , details []
                        ( summary [] [text "Error details"]
                        :: List.map
                            (p []
                                << List.singleton
                                << text
                                << Json.Decode.errorToString)
                            ds
                        )
                    ]

        _ ->
            div [] []


verdict : Config -> Tableau -> Html msg
verdict config t =
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
                , text (textVerdict config <| Zipper.zipper t)
                , text ":"
                ]
            , p []
                [ text (premises |> List.map (Formula.Signed.getFormula >> Formula.toString) |> String.join " , ")
                , text " ⊦ "
                , text (conclusions |> List.map (Formula.Signed.getFormula >> Formula.toString) |> String.join " , ")
                ]
            ]


textVerdict : Config -> Zipper -> String
textVerdict config t =
    case Helper.isClosed config t of
        Ok True ->
            "proves"

        Ok False ->
            "does not prove"

        Err _ ->
            "might be proving (once correct)"
