port module Editor exposing (Model, Msg, init, subscriptions, update, view, viewEmbeddable)

--, FileReaderPortData, fileContentRead, fileSelected

import Browser
import Config exposing (Config)
import Dict exposing (Dict)
import Errors
import File exposing (File)
import File.Download as Download
import File.Select as Select
import FontAwesome exposing (ellipsisHorizontal, exchangeAlt, icon, iconWithOptions)
import Formula exposing (Formula(..))
import Formula.Parser
import Formula.Signed exposing (Signed(..))
import Helpers.Exporting.Json.Decode
import Helpers.Exporting.Json.Encode
import Helpers.Rules as Rules exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Json.Encode exposing (set)
import LogicContext exposing (ContextData, LogicContext, createContext)
import Set
import Tableau exposing (..)
import Task
import UndoList exposing (UndoList)
import Validation
import Validation.Common exposing (Problem, ProblemType(..))
import Zipper exposing (..)


type JsonImport
    = None
    | InProgress String
    | ImportErr String (List Json.Decode.Error)


type alias State =
    { tableau : Tableau
    , jsonImport : JsonImport
    , config : Config
    }


type alias Model =
    { undoList : UndoList State
    , logicContext : Result String LogicContext
    }


init : Maybe Json.Decode.Value -> ( Model, Cmd msg )
init mtv =
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

        ( initCfg, initT ) =
            case mtv of
                Nothing ->
                    ( Config.default, emptyT )

                Just tv ->
                    Helpers.Exporting.Json.Decode.decodeValue tv
                        |> (\( cfg, t ) ->
                                ( cfg |> Result.withDefault Config.default, t |> Result.withDefault emptyT )
                           )
    in
    ( Model
        (UndoList.fresh
            { tableau = initT
            , jsonImport = None
            , config = initCfg
            }
        )
        (Err "not-loaded")
    , onChange { proofVerdict = contextVerdict initCfg (Zipper initT [] <| Err "not-loaded"), initial = True }
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ storeTrigger (\_ -> Store), updateContext UpdateContext ]


type Msg
    = ChangeText Zipper String
    | ChangeRef Zipper String
    | Delete Zipper
    | DeleteMe Zipper
    | MakeClosed Zipper
    | SetClosed Int Zipper String
    | MakeOpen Zipper
    | MakeOpenComplete Zipper
    | ExpandUnary Tableau.UnaryExtType Zipper
    | ExpandUnaryWithSubst Tableau.UnaryWithSubstExtType Zipper
    | ExpandBinary Tableau.BinaryExtType Zipper
    | ChangeSubst Zipper String
    | SwitchBetas Zipper
    | ChangeToUnary Tableau.UnaryExtType Zipper
    | ChangeToUnaryWithSubst Tableau.UnaryWithSubstExtType Zipper
    | ChangeToBinary Tableau.BinaryExtType Zipper
    | ChangeButtonsAppearance Zipper
    | SetConfig Config.Config
    | Undo
    | Redo
    | Prettify
    | JsonSelect
    | JsonSelected File
    | JsonRead String
    | Export
    | Print
    | Store
    | UpdateContext ContextData


port onPrint : () -> Cmd msg


port onStore : Json.Encode.Value -> Cmd msg


port onChange : { proofVerdict : Bool, initial : Bool } -> Cmd msg


port storeTrigger : (() -> msg) -> Sub msg


port updateContext : (ContextData -> msg) -> Sub msg


port onProofVerdict : Bool -> Cmd msg


top : Zipper -> Tableau
top =
    Zipper.top >> Zipper.zTableau


topRenumbered : Zipper -> Tableau
topRenumbered =
    top >> Zipper.renumber


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case doUpdate msg model of
        ( newModel, Nothing ) ->
            ( newModel
            , onChange
                { proofVerdict = contextVerdict newModel.undoList.present.config (Zipper newModel.undoList.present.tableau [] newModel.logicContext)
                , initial = False
                }
            )

        ( newModel, Just cmd ) ->
            ( newModel, cmd )


doUpdate : Msg -> Model -> ( Model, Maybe (Cmd Msg) )
doUpdate msg ({ undoList } as model) =
    let
        present =
            undoList.present
    in
    case msg of
        JsonSelect ->
            ( model
            , Just <| Select.file [ "application/json" ] JsonSelected
            )

        JsonSelected file ->
            ( { model
                | undoList =
                    { undoList
                        | present =
                            { present
                                | jsonImport = InProgress (File.name file)
                            }
                    }
              }
            , Just <| Task.perform JsonRead (File.toString file)
            )

        JsonRead contents ->
            case contents |> Json.Decode.decodeString Json.Decode.value of
                Ok value ->
                    case value |> Helpers.Exporting.Json.Decode.decodeValue of
                        ( Ok cfg, Ok t ) ->
                            ( { model
                                | undoList =
                                    UndoList.new
                                        { present
                                            | jsonImport = None
                                            , config = cfg
                                            , tableau = t
                                        }
                                        undoList
                              }
                            , Nothing
                            )

                        ( Err cfgErr, Ok t ) ->
                            ( { model
                                | undoList =
                                    UndoList.new
                                        { present
                                            | jsonImport =
                                                ImportErr
                                                    ("Failed to import rule set configuration. "
                                                        ++ "Keeping the last one."
                                                    )
                                                    [ cfgErr ]
                                            , tableau = t
                                        }
                                        undoList
                              }
                            , Nothing
                            )

                        ( Ok _, Err tErr ) ->
                            ( { model
                                | undoList =
                                    { undoList
                                        | present =
                                            { present
                                                | jsonImport =
                                                    ImportErr
                                                        "Failed to import tableau"
                                                        [ tErr ]
                                            }
                                    }
                              }
                            , Just <| Cmd.none
                            )

                        ( Err cfgErr, Err tErr ) ->
                            ( { model
                                | undoList =
                                    { undoList
                                        | present =
                                            { present
                                                | jsonImport =
                                                    ImportErr
                                                        ("Failed to import tableau and "
                                                            ++ "rule set configuration"
                                                        )
                                                        [ tErr, cfgErr ]
                                            }
                                    }
                              }
                            , Just <| Cmd.none
                            )

                Err err ->
                    ( { model
                        | undoList =
                            { undoList
                                | present =
                                    { present
                                        | jsonImport =
                                            ImportErr
                                                ("Failed to import file, "
                                                    ++ "its content is not valid JSON"
                                                )
                                                [ err ]
                                    }
                            }
                      }
                    , Just <| Cmd.none
                    )

        Export ->
            ( model
            , Just <|
                Download.string
                    "tableau.json"
                    "application/json"
                <|
                    Helpers.Exporting.Json.Encode.encodeString 2 present.config present.tableau
            )

        Undo ->
            ( { model
                | undoList =
                    UndoList.undo
                        { undoList | present = { present | jsonImport = None } }
              }
            , Nothing
            )

        Redo ->
            ( { model | undoList = UndoList.redo undoList }, Nothing )

        Print ->
            ( model, Just <| onPrint () )

        Store ->
            ( model
            , Just <|
                onStore <|
                    Helpers.Exporting.Json.Encode.encodeValue
                        undoList.present.config
                        undoList.present.tableau
            )

        UpdateContext ctx ->
            ( { model | logicContext = createContext ctx }, Nothing )

        _ ->
            let
                presentWithoutImport =
                    { present | jsonImport = None }
            in
            ( { model
                | undoList =
                    UndoList.new
                        (simpleUpdate msg presentWithoutImport)
                        { undoList | present = presentWithoutImport }
              }
            , Nothing
            )


simpleUpdate : Msg -> State -> State
simpleUpdate msg model =
    case msg of
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

        MakeOpenComplete z ->
            { model | tableau = z |> Zipper.makeOpenComplete |> top }

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

        Store ->
            model

        UpdateContext _ ->
            model


contextError : Result String LogicContext -> Html msg
contextError rctx =
    case rctx of
        Ok _ ->
            div [] []

        Err e ->
            case e of
                "not-loaded" ->
                    div [] []

                _ ->
                    div [ class "contextLoadError" ] [ text ("Context load error: " ++ e) ]


view : Model -> Browser.Document Msg
view model =
    { title = "Tableau Editor"
    , body = [ viewEmbeddable model ]
    }


viewEmbeddable : Model -> Html Msg
viewEmbeddable ({ undoList, logicContext } as model) =
  let
      z = Zipper undoList.present.tableau [] logicContext 
  in
  
    div [ class "tableauEditor" ]
        [ div [ class "actions" ]
            [ configMenu undoList.present.config
            , button [ class "button", onClick Prettify ] [ text "Prettify formulas" ]
            , button [ class "button", onClick Print ] [ text "Print" ]
            , jsonExportControl undoList.present.tableau
            , jsonImportControl undoList.present.jsonImport
            , button [ class "button", onClick Undo ] [ text "Undo" ]
            , button [ class "button", onClick Redo ] [ text "Redo" ]
            ]
        , jsonImportError undoList.present.jsonImport
        , contextError logicContext
        , div [ class "tableau" ]
            [ viewNode undoList.present.config z ]
        , verdict undoList.present.config z
        , problems undoList.present.config z
        , Rules.help undoList.present.config
        ]


viewNode : Config -> Zipper -> Html Msg
viewNode config z =
    div
        [ class "formula" ]
        [ viewNodeInputs identity config z
        , singleNodeProblems config z
        , viewControls config z
        , viewChildren config z
        ]


viewSubsNode : Config -> Zipper -> Html Msg
viewSubsNode config z =
    div [ class "formula" ]
        [ viewNodeInputs
            (\rest ->
                text "{"
                    :: autoSizeInput
                        (z |> up |> Zipper.zSubstitution |> Maybe.map .str |> Maybe.withDefault "")
                        [ classList
                            [ ( "textInput textInputSubst", True )
                            , ( "semanticsProblem", hasReference z )
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
    -> Zipper
    -> Html Msg
viewNodeInputs additional config z =
    div [ class "inputGroup" ]
        (text ("(" ++ ((Zipper.zNode z).id |> String.fromInt) ++ ")")
            :: autoSizeInput
                (Zipper.zNode z).value
                [ classList
                    [ ( "textInputFormula", True )
                    , ( "assumption", isAssumption z )
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
        List.filterMap unaryItem [ Assumption, Alpha ]
            ++ List.filterMap binaryItem [ Beta ]
            ++ List.filterMap unaryWithSubstItem [ Gamma, Delta, GammaStar, DeltaStar ]
            ++ List.filterMap unaryItem [ Refl, Leibnitz, MP, MT, HS, DS, NCS, ESFF, ESFT, ESTF, ESTT ]
            ++ List.filterMap binaryItem [ Cut, ECDF, ECDT ]


menuItem : Msg -> String -> Html Msg
menuItem msg str =
    li [] [ button [ onClick msg ] [ text str ] ]


menu cls label content =
    div [ class <| "onclick-menu " ++ cls, tabindex 0 ]
        [ span [] [ label ]
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
            :: onBlur Store
            :: attrs
        )
        []


viewRuleType : Zipper -> Html Msg
viewRuleType z =
    if isAssumption z then
        span [] [ var [] [ text "S" ], text "⁺" ]

    else
        case (Zipper.zTableau <| Zipper.up z).ext of
            Open ->
                text "O"

            Closed _ _ ->
                text "C"

            OpenComplete ->
                text "OC"

            Unary extType _ ->
                text (unaryExtTypeToString extType)

            Binary extType _ _ ->
                text (binaryExtTypeToString extType)

            UnaryWithSubst extType _ _ ->
                text (unaryWithSubstExtTypeToString extType)


viewButtonsAppearanceControlls : Zipper -> Html Msg
viewButtonsAppearanceControlls z =
    case (Zipper.zTableau z).ext of
        Closed _ _ ->
            div [] []

        OpenComplete ->
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


viewChildren : Config -> Zipper -> Html Msg
viewChildren config z =
    case (Zipper.zTableau z).ext of
        Open ->
            viewLeaf

        Closed r1 r2 ->
            viewLeaf

        OpenComplete ->
            viewLeaf

        Unary _ _ ->
            viewUnary config z

        UnaryWithSubst _ _ _ ->
            viewUnaryWithSubst config z

        Binary _ _ _ ->
            viewBinary config z


viewUnary : Config -> Zipper -> Html Msg
viewUnary config z =
    div [ class "alpha" ] [ viewNode config (Zipper.down z) ]


viewUnaryWithSubst : Config -> Zipper -> Html Msg
viewUnaryWithSubst config z =
    div [ class "alpha", class "withSubstitution" ] [ viewSubsNode config (Zipper.down z) ]


viewBinary : Config -> Zipper -> Html Msg
viewBinary config z =
    div [ class "beta" ]
        [ viewNode config (Zipper.left z)
        , viewNode config (Zipper.right z)
        ]


viewLeaf : Html Msg
viewLeaf =
    div [] []


viewControls : Config -> Zipper -> Html Msg
viewControls config ({ tableau } as z) =
    div [ class "expandControls" ]
        (case z.tableau.ext of
            Closed r1 r2 ->
                controlsClosed r1 r2 z

            OpenComplete ->
                controlsOpenComplete z

            _ ->
                let
                    addDeleteMeItem items =
                        li []
                            [ button [ onClick (DeleteMe z) ]
                                [ text "Delete node" ]
                            ]
                            :: items

                    addOptionalDeleteMeItem items =
                        if (z |> Zipper.up) /= z then
                            case z |> Zipper.up |> Zipper.zTableau |> .ext of
                                Binary _ _ _ ->
                                    case tableau.node.value of
                                        "" ->
                                            case tableau.ext of
                                                Open ->
                                                    addDeleteMeItem items

                                                _ ->
                                                    items

                                        _ ->
                                            items

                                _ ->
                                    addDeleteMeItem items

                        else
                            case tableau.ext of
                                Unary Alpha _ ->
                                    addDeleteMeItem items

                                Open ->
                                    addDeleteMeItem items

                                _ ->
                                    items

                    switchBetasButton =
                        case tableau.ext of
                            Binary _ _ _ ->
                                [ button [ class "button", onClick (SwitchBetas z), title "Swap branches" ] [ icon exchangeAlt ]
                                ]

                            _ ->
                                []
                in
                if tableau.node.gui.controlsShown then
                    button
                        [ class "button"
                        , onClick (ExpandUnary Assumption z)
                        ]
                        [ text "Add assumption" ]
                        :: ruleMenu
                            ExpandUnary
                            ExpandUnaryWithSubst
                            ExpandBinary
                            (text "Add")
                            "Add"
                            "add"
                            config
                            z
                        :: menu "del"
                            (text "Delete")
                            (addOptionalDeleteMeItem
                                [ li []
                                    [ button [ onClick (Delete z) ]
                                        [ text "Delete subtree" ]
                                    ]
                                ]
                            )
                        :: button [ class "button", onClick (MakeClosed z) ]
                            [ text "Close" ]
                        :: button
                            [ class "button"
                            , onClick (MakeOpenComplete z)
                            , title "Mark branch as open and complete"
                            ]
                            [ text "O&C" ]
                        :: switchBetasButton

                else
                    []
        )


controlsClosed : Ref -> Ref -> Zipper -> List (Html Msg)
controlsClosed r1 r2 z =
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
    , makeOpenButton "closed" z
    ]


controlsOpenComplete : Zipper -> List (Html Msg)
controlsOpenComplete z =
    let
        cls =
            problemsClass <| Errors.errors <| Validation.isBranchOpenComplete z
    in
    [ span [ class cls ] [ text "O&C\u{00A0}" ]
    , makeOpenButton "open and complete" z
    ]


makeOpenButton : String -> Zipper -> Html Msg
makeOpenButton currentState z =
    button
        [ class "button"
        , onClick (MakeOpen z)
        , title ("Unmark as " ++ currentState)
        ]
        [ text "×" ]


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


problems : Config -> Zipper -> Html Msg
problems config z =
    let
        errors =
            Errors.errors <| Validation.isCorrectTableau config <| z
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
            div [ class "jsonImportError" ] <|
                List.singleton <|
                    div []
                        [ p [] [ text <| "Import error: " ++ msg ]
                        , details []
                            (summary [] [ text "Error details" ]
                                :: List.map
                                    (p []
                                        << List.singleton
                                        << text
                                        << Json.Decode.errorToString
                                    )
                                    ds
                            )
                        ]

        _ ->
            div [] []


contextVerdict : Config -> Zipper -> Bool
contextVerdict config z =
    let
        ass =
            z |> assumptions
    in
    if List.isEmpty ass then
        False

    else
        case Validation.isClosed config z of
            Ok True ->
                True

            _ ->
                False


verdict : Config -> Zipper -> Html msg
verdict config z =
    let
        ass =
            z |> assumptions

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
                , text (textVerdict config z)
                , text ":"
                ]
            , p []
                [ text (premises |> List.map (Formula.Signed.getFormula >> Formula.toString) |> String.join " , ")
                , text " ⊦ "
                , text (conclusions |> List.map (Formula.Signed.getFormula >> Formula.toString) |> String.join " , ")
                ]
            ]


textVerdict : Config -> Zipper -> String
textVerdict config z =
    case Validation.isClosed config z of
        Ok True ->
            "proves"

        Ok False ->
            "does not prove"

        Err _ ->
            "might be proving (once correct)"
