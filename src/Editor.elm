port module Editor exposing (..)

import Errors
import Formula
import Helper
import Helpers.Exporting.Json.Decode
import Helpers.Exporting.Json.Encode
import Helpers.Exporting.Ports exposing (FileReaderPortData, fileContentRead, fileSelected)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode
import Rules exposing (..)
import Tableau exposing (..)
import UndoList exposing (UndoList)
import Validate
import Zipper exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    UndoList
        { tableau : Tableau
        , jsonImporting : Bool
        , jsonImportError : String
        , jsonImportId : String
        }


init : ( Model, Cmd msg )
init =
    ( UndoList.fresh
        { tableau =
            { node =
                { id = 1
                , value = ""
                , reference = { str = "1", up = Just 0 }
                , formula = Formula.parseSigned ""
                , gui = defGUI
                }
            , ext = Open
            }
        , jsonImporting = False
        , jsonImportError = ""
        , jsonImportId = "importJson"
        }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    fileContentRead JsonRead


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
    | Undo
    | Redo
    | Prettify
    | JsonSelected
    | JsonRead FileReaderPortData
    | ChangeButtonsAppearance Zipper.Zipper


top : Zipper.Zipper -> Tableau
top =
    Zipper.top >> Zipper.zTableau


topRenumbered : Zipper.Zipper -> Tableau
topRenumbered =
    top >> Zipper.renumber


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ present } as model) =
    case msg of
        JsonSelected ->
            ( UndoList.new { present | jsonImportError = "", jsonImporting = True } model, fileSelected present.jsonImportId )

        Undo ->
            ( UndoList.undo model, Cmd.none )

        Redo ->
            ( UndoList.redo model, Cmd.none )

        _ ->
            ( UndoList.new (simpleUpdate msg { present | jsonImportError = "" }) model, Cmd.none )



--simpleUpdate : Msg -> Model -> (Model)


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

            Prettify ->
                { model | tableau = Zipper.prettify model.tableau }

            JsonSelected ->
                model

            Undo ->
                model

            Redo ->
                model

            JsonRead { contents } ->
                case contents |> Helpers.Exporting.Json.Decode.decode of
                    Ok t ->
                        { model | jsonImporting = False, tableau = t }

                    Err e ->
                        { model | jsonImporting = False, jsonImportError = toString e }

            ChangeButtonsAppearance z ->
                { model | tableau = z |> Zipper.changeButtonAppearance |> top }
        )


view : Model -> Html Msg
view ({ present } as model) =
    div [ class "tableau" ]
        [ div [ class "actions" ]
            [ button [ class "button", onClick Prettify ] [ text "Prettify formulas" ]
            , button [ class "button", attribute "onClick" "javascript:window.print()" ] [ text "Print" ]
            , jsonExportControl present.tableau
            , jsonImportControl present.jsonImporting present.jsonImportId
            , button [ class "button", onClick Undo ] [ text "Undo" ]
            , button [ class "button", onClick Redo ] [ text "Redo" ]
            ]
        , jsonImportError present
        , viewNode (Zipper.zipper present.tableau)
        , verdict present.tableau
        , problems present.tableau
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
        , viewButtonsAppearanceControlls z
        , singleNodeProblems z
        , viewControls z
        , viewChildren z
        ]


viewButtonsAppearanceControlls : Zipper.Zipper -> Html Msg
viewButtonsAppearanceControlls z =
    button [ class "button", onClick (ChangeButtonsAppearance z) ] [ text "E" ]


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
        , text "|"
        , input
            [ classList
                [ ( "substitutedVariable", True )
                , ( "semanticsProblem", Helper.hasReference z )
                ]
            , value (z |> up |> Zipper.zSubstitution |> Maybe.map .what |> Maybe.withDefault "")
            , type_ "text"
            , onInput <| ChangeTerm z
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
            , onInput <| ChangeVariable z
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
        , viewButtonsAppearanceControlls z
        , singleNodeProblems z
        , viewControls z
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
    div [] []


viewClosed : Zipper.Zipper -> Html Msg
viewClosed z =
    div [] []


viewControls : Zipper.Zipper -> Html Msg
viewControls z =
    let
        ( t, bs ) =
            z
    in
    div [ class "expandControls" ]
        (case t.ext of
            Tableau.Closed r1 r2 ->
                let
                    compl =
                        Errors.errors <| Validate.areCloseRefsComplementary r1 r2 z

                    ref1Cls =
                        problemsClass <| Validate.validateRef "Invalid close ref. #1" r1 z ++ compl

                    ref2Cls =
                        problemsClass <| Validate.validateRef "Invalid close ref. #1" r2 z ++ compl
                in
                [ text "* "
                , input
                    [ class ("closed button " ++ ref1Cls)
                    , type_ "text"
                    , placeholder "Ref"
                    , size 1
                    , value r1.str
                    , onInput <| SetClosed 0 z
                    ]
                    []
                , input
                    [ class ("closed button " ++ ref2Cls)
                    , type_ "text"
                    , placeholder "Ref"
                    , size 1
                    , value r2.str
                    , onInput <| SetClosed 1 z
                    ]
                    []
                , button [ class "button", onClick (MakeOpen z) ] [ text "o" ]
                ]

            Tableau.Beta lt rt ->
                case t.node.gui.controlsShown of
                    True ->
                        [ div [ class "onclick-menu add", tabindex 0 ]
                            [ ul [ class "onclick-menu-content" ]
                                [ li [] [ button [ onClick (ExpandAlpha z) ] [ text "α" ] ]
                                , li [] [ button [ onClick (ExpandBeta z) ] [ text "β" ] ]
                                , li [] [ button [ onClick (ExpandGamma z) ] [ text "γ" ] ]
                                , li [] [ button [ onClick (ExpandDelta z) ] [ text "δ" ] ]
                                ]
                            ]
                        , div [ class "onclick-menu change", tabindex 0 ]
                            [ ul [ class "onclick-menu-content" ]
                                [ li [] [ button [ onClick (ChangeToAlpha z) ] [ text "α" ] ]
                                , li [] [ button [ onClick (ChangeToBeta z) ] [ text "β" ] ]
                                , li [] [ button [ onClick (ChangeToGamma z) ] [ text "γ" ] ]
                                , li [] [ button [ onClick (ChangeToDelta z) ] [ text "δ" ] ]
                                ]
                            ]
                        , div [ class "onclick-menu del", tabindex 0 ]
                            [ ul [ class "onclick-menu-content" ]
                                [ li [] [ button [ onClick (DeleteMe z) ] [ text "node" ] ]
                                , li [] [ button [ onClick (Delete z) ] [ text "subtree" ] ]
                                ]
                            ]
                        , button [ class "button", onClick (MakeClosed z) ] [ text "*" ]
                        , button [ class "button", onClick (SwitchBetas z) ] [ text "->|<-" ]
                        ]

                    False ->
                        []

            _ ->
                case t.node.gui.controlsShown of
                    True ->
                        [ div [ class "onclick-menu add", tabindex 0 ]
                            [ ul [ class "onclick-menu-content" ]
                                [ li [] [ button [ onClick (ExpandAlpha z) ] [ text "α" ] ]
                                , li [] [ button [ onClick (ExpandBeta z) ] [ text "β" ] ]
                                , li [] [ button [ onClick (ExpandGamma z) ] [ text "γ" ] ]
                                , li [] [ button [ onClick (ExpandDelta z) ] [ text "δ" ] ]
                                ]
                            ]
                        , div [ class "onclick-menu change", tabindex 0 ]
                            [ ul [ class "onclick-menu-content" ]
                                [ li [] [ button [ onClick (ChangeToAlpha z) ] [ text "α" ] ]
                                , li [] [ button [ onClick (ChangeToBeta z) ] [ text "β" ] ]
                                , li [] [ button [ onClick (ChangeToGamma z) ] [ text "γ" ] ]
                                , li [] [ button [ onClick (ChangeToDelta z) ] [ text "δ" ] ]
                                ]
                            ]
                        , div [ class "onclick-menu del", tabindex 0 ]
                            [ ul [ class "onclick-menu-content" ]
                                [ li [] [ button [ onClick (DeleteMe z) ] [ text "node" ] ]
                                , li [] [ button [ onClick (Delete z) ] [ text "subtree" ] ]
                                ]
                            ]
                        , button [ class "button", onClick (MakeClosed z) ] [ text "*" ]
                        ]

                    False ->
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


problemClass : { a | typ : Validate.ProblemType } -> String
problemClass { typ } =
    case typ of
        Validate.Syntax ->
            "syntaxProblem"

        Validate.Semantics ->
            "semanticsProblem"


jsonDataUri : String -> String
jsonDataUri json =
    "data:application/json;charset=utf-8," ++ Http.encodeUri json


jsonExportControl : Tableau -> Html msg
jsonExportControl t =
    a
        [ type_ "button"
        , href <| jsonDataUri <| Helpers.Exporting.Json.Encode.encode 2 t
        , downloadAs "tableau.json"
        ]
        [ button [ class "button" ] [ text "Export as JSON" ] ]


jsonImportControl : Bool -> String -> Html Msg
jsonImportControl jsonImporting jsonImportId =
    case jsonImporting of
        True ->
            text "Loading file..."

        False ->
            label [ for jsonImportId ]
                [ button
                    {- This is really ugly, but:
                       - we really need the buton and onClick, if we want it to look like a button
                         (embedding the label in a button or vice versa works in webkit but not in firefox)
                       - Adding another Msg / Cmd just for this...
                    -}
                    [ attribute "onClick" ("javascript:document.getElementById('" ++ jsonImportId ++ "').click();")
                    , class "button"
                    ]
                    [ text "Import from JSON"
                    ]
                , input
                    [ type_ "file"
                    , id jsonImportId
                    , accept "application/json"
                    , on "change"
                        (Json.Decode.succeed JsonSelected)
                    ]
                    []
                ]


jsonImportError : { a | jsonImportError : String } -> Html msg
jsonImportError model =
    case model.jsonImportError of
        "" ->
            div [] []

        _ ->
            p
                [ class "jsonImportError" ]
                [ text <| "Error importing tableau: " ++ toString model.jsonImportError ]


verdict : Tableau -> Html msg
verdict t =
    let
        ass =
            t |> Zipper.zipper |> Helper.assumptions

        ( premises, conclusions ) =
            List.partition
                (\sf ->
                    case sf of
                        Formula.T _ ->
                            True

                        Formula.F _ ->
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
                [ text (premises |> List.map (Formula.signedGetFormula >> Formula.strFormula) |> String.join " , ")
                , text " ⊦ "
                , text (conclusions |> List.map (Formula.signedGetFormula >> Formula.strFormula) |> String.join " , ")
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
