port module Editor exposing (main, top, topRenumbered)

--, FileReaderPortData, fileContentRead, fileSelected

import Browser
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
import Tableau exposing (..)
import Task
import UndoList exposing (UndoList)
import Validation
import Validation.Common exposing (Problem, ProblemType(..))
import Zipper exposing (..)
import Json.Encode exposing (set)


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
        , jsonImport : JsonImport
        }


init : Maybe String -> ( Model, Cmd msg )
init mts =
    let
        emptyT =
            { node =
                { id = 1
                , value = ""
                , references = [ ]
                , formula = Formula.Parser.parseSigned ""
                , gui = defGUI
                }
            , ext = Open
            }

        initT =
            case mts of
                Nothing ->
                    emptyT

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
              <|
                Helpers.Exporting.Json.Encode.encode 2 present.tableau
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
            , cache (Helpers.Exporting.Json.Encode.encode 0 model.present.tableau)
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
            z
        , singleNodeProblems z
        , viewControls z
        , viewChildren z
        ]


viewNodeInputs :
    (List (Html Msg) -> List (Html Msg))
    -> Zipper.Zipper
    -> Html Msg
viewNodeInputs additional z =
    div [ class "inputGroup" ]
        (text ("(" ++ ((Zipper.zNode z).id |> String.fromInt) ++ ")")
            :: autoSizeInput
                (Zipper.zNode z).value
                [ classList
                    [ ( "textInputFormula", True )
                    , ( "premise", Helper.isPremise z )
                    ]
                , class (errorsClass <| Validation.isCorrectFormula z)
                , type_ "text"
                , onInput <| ChangeText z
                ]
            :: viewRuleType z
            :: ruleMenu ChangeToUnary ChangeToUnaryWithSubst ChangeToBinary "▾" "Change to" "change" z
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


ruleMenu unaryMsg unaryWithSubstMsg binaryMsg label labelPrefix cls z =
    let
        unaryItem extType = menuItem (unaryMsg extType z) (labelPrefix ++ " " ++ (unaryExtTypeToString extType))
        unaryWithSubstItem extType = menuItem (unaryWithSubstMsg extType z) (labelPrefix ++ " " ++ (unaryWithSubstExtTypeToString extType))
        binaryItem extType = menuItem (binaryMsg extType z) (labelPrefix ++ " " ++ (binaryExtTypeToString extType))
    in
        div [ class  "onclick-menu" ]
            [button [class <| "onclick-menu " ++ cls, tabindex 0] [
                    text label,
                 ul [ class "onclick-menu-content"] <|
                    [ unaryItem Alpha
                    , binaryItem Beta
                    , unaryWithSubstItem Gamma
                    , unaryWithSubstItem Delta
                    ]
                    ++ List.map unaryItem [ Refl, Leibnitz, MP, MT ]
                    ++ binaryItem Cut
                    :: List.map unaryItem [ HS, DS, NCS, ESFF, ESFT, ESTF, ESTT ]
                    ++ List.map binaryItem [ ECDF, ECDT ]
            ]
            ]


menuItem : Msg -> String -> Html Msg
menuItem msg str = 
    li [] [ button [onClick msg] [ text str ] ]


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


viewChildren : Zipper.Zipper -> Html Msg
viewChildren z =
    case (Zipper.zTableau z).ext of
        Open ->
            viewOpen z

        Closed r1 r2 ->
            viewClosed z

        Unary _ _ ->
            viewUnary z

        UnaryWithSubst _ _ _ ->
            viewUnaryWithSubst z

        Binary _ _ _ ->
            viewBinary z


viewUnary : Zipper.Zipper -> Html Msg
viewUnary z =
    div [ class "alpha" ] [ viewNode (Zipper.down z) ]

viewUnaryWithSubst : Zipper.Zipper -> Html Msg
viewUnaryWithSubst z =
    div [ class "alpha" ] [ viewSubsNode (Zipper.down z) ]


viewBinary : Zipper.Zipper -> Html Msg
viewBinary z =
    div [ class "beta" ]
        [ viewNode (Zipper.left z)
        , viewNode (Zipper.right z)
        ]


viewOpen : Zipper.Zipper -> Html Msg
viewOpen z =
    div [] []


viewClosed : Zipper.Zipper -> Html Msg
viewClosed z =
    div [] []


viewControls : Zipper.Zipper -> Html Msg
viewControls (( t, _ ) as z) =
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
                    , ruleMenu ExpandUnary ExpandUnaryWithSubst ExpandBinary "Add ▾" "Add" "add" z
                    , div [ class "onclick-menu" ]
                        [ button [ class "onclick-menu del", tabindex 0 ] [
                            text "Delete ▾",
                            ul [ class "onclick-menu-content" ]
                            [ li [] [ deleteMeButton ]
                            , li [] [ button [ onClick (Delete z) ] [ text "Delete subtree" ] ]
                            ]
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
            Errors.errors <| Validation.isCorrectNode <| z
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
            Errors.errors <| Validation.isCorrectTableau <| Zipper.zipper <| t
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
