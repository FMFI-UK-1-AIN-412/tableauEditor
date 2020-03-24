port module Editor exposing (FileReaderPortData, Model, Msg(..), enableDebug, errorClass, expandControls, selectFile, fileContentRead, fileSelected, cache, init, isBeta, isPremise, jsonDataUri, jsonExportControl, jsonImportControl, jsonImportError, main, problemClass, problemColor, problemItem, problemList, problems, problemsClass, simpleUpdate, subscriptions, tblCell, tblRow, textVerdict, top, topRenumbered, update, verdict, view, viewFormula, viewTableau)

import Errors
import Formula exposing (Formula)
import Help
import Browser
import Html exposing (Attribute, Html, h1, a, button, div, input, label, li, p, pre, span, table, td, text, tr, ul, var, sub, sup)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, onBlur)
import Html.Lazy
import HtmlFormula exposing (htmlFormula)
import Url
import Json.Decode
import Result
import Tableau exposing (..)
import Tableau.Closed exposing (assumptions, isClosed)
import Tableau.Json.Decode
import Tableau.Json.Encode
import Validate exposing (..)
import Debug exposing (toString)
import String


enableDebug =
    False


main =
    Browser.document
        { init = init
        , view = documentView
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { t : Tableau.Tableau
    , jsonImporting : Bool
    , jsonImportError : String
    , jsonImportId : String
    }

init : Maybe String -> (Model, Cmd Msg)
init mts =
    let
        emptyT = Tableau.Leaf { num = 1, text = "", ref = { str = "1", up = Just 0 } } Unfinished

        initT =
            case mts of
                Nothing -> emptyT
                Just ts ->
                    case Tableau.Json.Decode.decode ts of
                        Ok t ->
                            t

                        Err e ->
                            emptyT
    in
    
    ( { t = initT
      , jsonImporting = False
      , jsonImportError = ""
      , jsonImportId = "importJson"
      }
    , Cmd.none
    )


type Msg
    = Text Tableau.Zipper String
    | Ref Tableau.Zipper String
    | ExpandAlpha Tableau.Zipper
    | ExpandBeta Tableau.Zipper
    | MakeClosed Tableau.Zipper
    | MakeUnfinished Tableau.Zipper
    | MakeOpenComplete Tableau.Zipper
    | SetClosed Int Tableau.Zipper String
    | Delete Tableau.Zipper
    | Prettify
    | Print
    | SelectJson
    | JsonSelected
    | JsonRead FileReaderPortData
    | Cache


type alias FileReaderPortData =
    { contents : String
    , filename : String
    }


port selectFile : String -> Cmd msg


port fileSelected : String -> Cmd msg


port fileContentRead : (FileReaderPortData -> msg) -> Sub msg


port print : () -> Cmd msg


port cache : String -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    fileContentRead JsonRead


top =
    Tableau.top >> Tableau.zTableau


topRenumbered =
    top >> Tableau.renumber


simpleUpdate : Msg -> Model -> Model
simpleUpdate msg model =
    case msg of
        ExpandAlpha z ->
            { model | t = z |> Tableau.extendAlpha |> topRenumbered }

        ExpandBeta z ->
            { model | t = z |> Tableau.extendBeta |> topRenumbered }

        MakeClosed z ->
            { model | t = z |> Tableau.makeClosed |> top }

        MakeOpenComplete z ->
            { model | t = z |> Tableau.makeOpenComplete |> top }

        MakeUnfinished z ->
            { model | t = z |> Tableau.makeUnfinished |> top }

        Delete z ->
            { model | t = z |> Tableau.delete |> topRenumbered }

        Text z txt ->
            { model | t = z |> Tableau.setFormula txt |> top }

        Ref z ref ->
            { model | t = z |> Tableau.setRef ref |> top }

        SetClosed w z ref ->
            { model | t = z |> Tableau.setClosed w ref |> top }

        Prettify ->
            { model | t = Tableau.prettify model.t }

        -- actually handled upstairs
        JsonRead { contents } ->
            case contents |> Tableau.Json.Decode.decode of
                Ok t ->
                    { model | jsonImporting = False, t = t }

                Err e ->
                    { model | jsonImporting = False, jsonImportError = toString e }

        SelectJson ->
            model

        JsonSelected ->
            model

        Print ->
            model

        Cache ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        JsonSelected ->
            ( { model | jsonImportError = "", jsonImporting = True }, fileSelected model.jsonImportId )

        SelectJson ->
            ( model, selectFile model.jsonImportId )

        JsonRead { contents } ->
            case contents |> Tableau.Json.Decode.decode of
                Ok t ->
                    ( { model | jsonImporting = False, t = t }
                    , cache contents
                    )

                Err e ->
                    ( { model
                        | jsonImporting = False
                        , jsonImportError = toString e }
                    , Cmd.none)

        Print ->
            ( model, print () )

        Cache ->
            ( model, cache (Tableau.Json.Encode.encode 2 model.t) )

        _ ->
            ( simpleUpdate msg { model | jsonImportError = "" }, Cmd.none )

documentView : Model -> Browser.Document Msg
documentView model =
    { title = "Tableau Editor"
    , body = [ view model ]
    }

view : Model -> Html Msg
view model =
    div []
        [ h1 [] [text "Tableau Editor"]
        , Html.Lazy.lazy viewTableau model.t
        , Html.Lazy.lazy problems model.t
        , Html.Lazy.lazy verdict model.t
        , p [ class "actions" ]
            [ button [ onClick Prettify ] [ text "Prettify formulas" ]
            , button [ onClick Print ] [ text "Print" ]
            , Html.Lazy.lazy jsonExportControl model.t
            , jsonImportControl model
            ]
        , jsonImportError model
        , Help.help
        , div []
            (if enableDebug then
                [ p [] [ text "Debug" ]
                , pre []
                    [ text (Tableau.indented 2 model.t)
                    ]
                , p []
                    [ text (toString model) ]
                ]

             else
                []
            )
        ]


verdict t =
    let
        ass =
            t |> Tableau.zipper |> assumptions

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

        htmlUnSignedFormulas =
            HtmlFormula.joinMapA (text ", ")
                (HtmlFormula.htmlFormula << Formula.signedGetFormula)
    in
    if List.isEmpty ass then
        div [ class "verdict" ] [ p [] [ text "This tableau doesn't prove anything." ] ]

    else
        div [ class "verdict" ]
            [ p []
                [ text "This tableau "
                , text (textVerdict <| Tableau.zipper t)
                , text ":"
                ]
            , p [ class "provable" ] <|
                htmlUnSignedFormulas premises <|
                    text " ⊢"
                        :: sub [] [text "p"]
                        :: text " "
                        :: htmlUnSignedFormulas conclusions []
            ]


textVerdict t =
    case isClosed t of
        Ok True ->
            "proves"

        Ok False ->
            "does not prove"

        Err _ ->
            "might be proving (once correct)"


problems t =
    let
        errors =
            Errors.errors <| isCorectTableau <| Tableau.zipper <| t
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
        , text <| String.fromInt <| .num <| Tableau.zNode <| pi.zip
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


errorClass : Result (List Validate.Problem) a -> String
errorClass =
    Errors.errors >> problemsClass


problemClass { typ } =
    case typ of
        Validate.Syntax ->
            "syntaxProblem"

        Validate.Semantics ->
            "semanticsProblem"


problemColor : Validate.Problem -> String
problemColor p =
    case p.typ of
        Validate.Syntax ->
            "lightpink"

        Validate.Semantics ->
            "yellow"


isPremise z =
    case z |> zNode |> .ref |> .up of
        Just 0 ->
            True

        _ ->
            False


isBeta ( t, _ ) =
    case t of
        Beta _ _ _ ->
            True

        _ ->
            False


viewTableau : Tableau.Tableau -> Html Msg
viewTableau tbl =
    let
        t =
            Tableau.asTable tbl
    in
    table [ class "tableau" ]
        (List.map2 (Html.Lazy.lazy2 tblRow)
            (List.reverse <| List.range 1 (List.length t))
            t
        )


tblRow : Int -> Tableau.Row -> Html Msg
tblRow depth trow =
    tr [] (List.map (tblCell depth) trow)


tblCell : Int -> Tableau.Cell -> Html Msg
tblCell depth tcell =
    let
        ( width, mz ) =
            tcell

        cellProps =
            case mz of
                Nothing ->
                    { content = []
                    , height = depth
                    , classes = []
                    , title = ""
                    }

                Just z ->
                    { content = viewFormula z :: expandControls z
                    , height = let
                                    ( t, bs ) =
                                        z
                                in
                                case t of
                                    Tableau.Leaf _ _ ->
                                        depth + 1

                                    _ ->
                                        1
                    , classes = [ ( "premise", isPremise z )
                                , ( "beta", isBeta z ) ]
                    , title = z
                            |> isCorrectNode
                            |> Errors.errors
                            |> List.map .msg
                            |> String.join " \n "
                    }
    in
    td
        [ classList cellProps.classes
        , colspan width
        , rowspan cellProps.height
        , title cellProps.title
        ]
        cellProps.content


viewFormula z =
    let
        ( t, bs ) =
            z

        n =
            Tableau.node t

        formula =
            Formula.parseSigned n.text
    in
    div
        [ class "formula"
        ]
        [ text <| "(" ++ String.fromInt n.num ++ ")"
        , input
            [ class
                ("formulaEdit "
                    ++ errorClass (isCorrectFormula z)
                )
            , type_ "text"
            , placeholder "Formula"
            , value n.text
            , size (String.length n.text * 3 // 4 + 1)
            , onInput <| Text z
            , onBlur <| Cache
            ]
            []
        , text " "
        , if isPremise z
            then span [] [ var [] [text "S"], sup [] [text "+"] ]
            else text (if isBeta <| up z then "β" else "α")
        , text "["
        , input
            [ class ("refEdit " ++ errorClass (isValidNodeRef z))
            , type_ "text"
            , placeholder "Ref"
            , size 1
            , value n.ref.str
            , onInput <| Ref z
            , onBlur <| Cache
            ]
            []
        , text "]"
        , deleteButton (Delete z)
        ]


deleteButton msg =
    button [ class "delete", onClick msg ] [ text "×" ]


expandControls z =
    let
        ( t, bs ) =
            z
    in
    [ div [ class "expandControls" ]
        (case t of
            Tableau.Leaf _ fin ->
                case fin of
                    Unfinished ->
                        [ button [ onClick (ExpandAlpha z) ] [ text "α" ]
                        , button [ onClick (ExpandBeta z) ] [ text "β" ]
                        , button [ onClick (MakeClosed z) ] [ text "*" ]
                        , button [ onClick (MakeOpenComplete z) ] [ text "O&C" ]
                        ]

                    Closed ( r1, r2 ) ->
                        let
                            compl =
                                Errors.errors <| areCloseRefsComplementary r1 r2 z

                            ref1Cls =
                                problemsClass <| validateRef "Invalid close ref. #1" r1 z ++ compl

                            ref2Cls =
                                problemsClass <| validateRef "Invalid close ref. #2" r2 z ++ compl
                        in
                        [ text "* "
                        , input
                            [ class ("refEdit " ++ ref1Cls)
                            , type_ "text"
                            , placeholder "Ref"
                            , size 1
                            , value r1.str
                            , onInput <| SetClosed 0 z
                            , onBlur <| Cache
                            ]
                            []
                        , input
                            [ class ("refEdit " ++ ref2Cls)
                            , type_ "text"
                            , placeholder "Ref"
                            , size 1
                            , value r2.str
                            , onInput <| SetClosed 1 z
                            , onBlur <| Cache
                            ]
                            []
                        , deleteButton (MakeUnfinished z)
                        ]

                    OpenComplete ->
                        let
                            cls = problemsClass
                                <| Errors.errors (isCorrectOpenComplete z)
                            ttl =
                                if String.isEmpty cls then
                                    [title "Open and complete branch"]
                                else
                                    []
                        in
                        
                            [ span
                                (class ("openComplete " ++ cls) :: ttl)
                                [ text "O&C" ]
                            , deleteButton (MakeUnfinished z)
                            ]

            Tableau.Alpha _ _ ->
                []

            Tableau.Beta _ _ _ ->
                []
        )
    ]


jsonDataUri json =
    "data:application/json;charset=utf-8," ++ Url.percentEncode json


jsonExportControl t =
    a
        [ type_ "button"
        , href <| jsonDataUri <| Tableau.Json.Encode.encode 2 t
        , download "tableau.json"
        ]
        [ button [] [ text "Export as JSON" ] ]



{-
   Note: selecting the same file on the file input won't
   trigger a change evet. So we remove the whole input while
   loading, so it gets re-added as empty...
   (which actually gives a nice "loading..." message)
   Ideally we should block all interaction while loading... but...
-}


jsonImportControl : Model -> Html Msg
jsonImportControl model =
    if model.jsonImporting
    then
        text "Loading file..."
    else
        label [ for model.jsonImportId ]
            [ button
                [ onClick SelectJson ]
                [ text "Import from JSON"
                ]
            , input
                [ type_ "file"
                , id model.jsonImportId
                , accept "application/json"
                , on "change"
                    (Json.Decode.succeed JsonSelected)
                ]
                []
            ]


jsonImportError model =
    case model.jsonImportError of
        "" ->
            div [] []

        _ ->
            p
                [ class "jsonImportError" ]
                [ text <| "Error importing tableau: " ++ toString model.jsonImportError ]



{- vim: set sw=2 ts=2 sts=2 et : -}
