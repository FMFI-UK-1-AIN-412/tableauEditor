port module Editor exposing (..)
import Html exposing
  (Html, Attribute, div, input, button, table, tr, td, text, pre, p, ul, li, a, span, label)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, on)
import Result
import Tableau exposing (..)
import Validate exposing (..)
import Tableau.Closed exposing (isClosed, assumptions)
import Errors
import Help
import Tableau.Json.Encode
import Tableau.Json.Decode
import Http
import Json.Decode

import Formula exposing (Formula)
import HtmlFormula exposing (htmlFormula)

enableDebug = False

main =
  Html.program
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

type alias Model =
  { t : Tableau.Tableau
  , jsonImporting : Bool
  , jsonImportError : String
  , jsonImportId : String
  }

init =
  ( { t = Tableau.Leaf { num = 1, text = "", ref = { str = "1", up = Just 0 } } Nothing
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
  | MakeOpen Tableau.Zipper
  | SetClosed Int Tableau.Zipper String
  | Delete Tableau.Zipper
  | Prettify
  | JsonSelected
  | JsonRead FileReaderPortData


type alias FileReaderPortData =
  { contents : String
  , filename : String
  }
port fileSelected : String -> Cmd msg
port fileContentRead : (FileReaderPortData -> msg) -> Sub msg
subscriptions : Model -> Sub Msg
subscriptions model =
 fileContentRead JsonRead

top = Tableau.top >> Tableau.zTableau
topRenumbered = top >> Tableau.renumber

simpleUpdate : Msg -> Model -> Model
simpleUpdate msg model =
  case msg of
    ExpandAlpha     z -> { model | t = z |> Tableau.extendAlpha     |> topRenumbered }
    ExpandBeta      z -> { model | t = z |> Tableau.extendBeta      |> topRenumbered }
    MakeClosed      z -> { model | t = z |> Tableau.makeClosed      |> top }
    MakeOpen        z -> { model | t = z |> Tableau.makeOpen        |> top }
    Delete          z -> { model | t = z |> Tableau.delete          |> topRenumbered }
    Text        z txt -> { model | t = z |> Tableau.setFormula txt  |> top }
    Ref         z ref -> { model | t = z |> Tableau.setRef ref      |> top }
    SetClosed w z ref -> { model | t = z |> Tableau.setClosed w ref |> top }
    Prettify          -> { model | t = Tableau.prettify model.t }
    JsonSelected -> model -- actually handled upstairs
    JsonRead {contents} ->
      case contents |> Tableau.Json.Decode.decode of
        Ok  t -> { model | jsonImporting = False, t = t }
        Err e -> { model | jsonImporting = False, jsonImportError = toString e }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    JsonSelected ->
      ( { model | jsonImportError = "", jsonImporting = True }, fileSelected model.jsonImportId)
    _ ->
      (simpleUpdate msg { model | jsonImportError = "" }, Cmd.none)


view : Model -> Html Msg
view model =
  div []
    [ viewTableau model.t
    , verdict model.t
    , problems model.t
    , p [ class "actions" ]
      [ button [ onClick Prettify ] [text "Prettify formulas"]
      , button [ attribute "onClick" "javascript:window.print()" ] [ text "Print" ]
      , jsonExportControl model.t
      , jsonImportControl model
      ]
    , jsonImportError model
    , Help.help
    , div []
      ( if enableDebug
        then
          [ p [] [text "Debug"]
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
    ass = t |> Tableau.zipper |> assumptions
    (premises, conclusions) =
      List.partition
        (\sf ->
          case sf of
            Formula.T _ -> True
            Formula.F _ -> False
        )
        ass
    htmlUnSignedFormulas =
      HtmlFormula.joinMapA (text ", ")
        (HtmlFormula.htmlFormula << Formula.signedGetFormula)

  in
    if List.isEmpty ass
      then div [ class "verdict" ] [ p [] [ text "This tableau doesn't prove anything." ] ]
      else
        div [ class "verdict" ]
        [ p []
          [ text "This tableau "
          , text (textVerdict <| Tableau.zipper t)
          , text ":"
          ]
        , p [] <|
            htmlUnSignedFormulas premises <|
            text " ⊦ " ::
            htmlUnSignedFormulas conclusions []
        ]

textVerdict t =
  case isClosed t of
    Ok True -> "proves"
    Ok False -> "does not prove"
    Err _ -> "might be proving (once correct)"

problems t =
  let
    errors = Errors.errors <| isCorectTableau <| Tableau.zipper <| t
  in
    if List.isEmpty errors
    then
      div [class "problems"] []
    else
      div [class "problems"]
      [ p [] [text "Problems"]
      , problemList <| errors
      ]

problemList : (List Validate.Problem) -> Html Msg
problemList pl = ul [ class "problemList" ] (List.map problemItem pl)

problemItem : Validate.Problem -> Html Msg
problemItem pi = li [ class (problemClass pi) ]
  [ text "("
  , text <| toString <| .num <| Tableau.zNode <| pi.zip
  , text ") "
  , text <| pi.msg
  ]

problemsClass : (List Validate.Problem) -> String
problemsClass pl =
  case pl of
    [] -> ""
    p::_ -> problemClass p

errorClass : Result (List Validate.Problem) a -> String
errorClass =
  Errors.errors >> problemsClass

problemClass { typ } =
  case typ of
    Validate.Syntax    -> "syntaxProblem"
    Validate.Semantics -> "semanticsProblem"

problemColor : Validate.Problem -> String
problemColor p =
  case p.typ of
    Validate.Syntax -> "lightpink"
    Validate.Semantics -> "yellow"

isPremise z =
  case z |> zNode |> .ref |> .up of
    Just 0 -> True
    _ -> False

isBeta (t, _) =
  case t of
    Beta _ _ _ -> True
    _ -> False

viewTableau : Tableau.Tableau -> Html Msg
viewTableau tbl=
  let
      t = Tableau.asTable tbl
  in
      table [class "tableau"] (List.map2 tblRow
        (List.reverse <| List.range 1 (List.length t))
        t
      )

tblRow : Int -> Tableau.Row -> (Html Msg)
tblRow depth trow =
  tr [] (List.map (tblCell depth) trow)

tblCell : Int -> Tableau.Cell -> (Html Msg)
tblCell depth tcell =
  let
      (width, mz) = tcell
      (content,height,clss,ttl) = case mz of
        Nothing -> ([],depth,[],"")
        Just z ->
          ( [ viewFormula z ] ++ expandControls z
          , let
              (t, bs) = z
            in
              case t of
                Tableau.Leaf _ _ -> depth + 1
                _ -> 1
          , [ ("premise", isPremise z), ("beta", isBeta z) ]
          , ( z |> isCorrectNode |> Errors.errors
              |> List.map .msg |> String.join " \n "
            )
          )
  in td
    [ classList clss
    , colspan width, rowspan height
    , title ttl
    ]
    content

viewFormula z =
  let
    (t, bs) = z
    n = Tableau.node t
    formula = Formula.parseSigned n.text
  in
    div
      [ class "formula"
      ]
      [ text <| "(" ++ (toString n.num) ++ ")"
      , input
        [ class ("formulaEdit "
            ++ errorClass (isCorrectFormula z)
            )
        , type_ "text"
        , placeholder "Formula"
        , value n.text
        , size ((String.length n.text) * 3 // 4 + 1)
        , onInput <| Text z
        ]
        []
      , text " ["
      , input
        [ class ("refEdit " ++ errorClass (isValidNodeRef z))
        , type_ "text"
        , placeholder "Ref"
        , size 1
        , value n.ref.str
        , onInput <| Ref z
        ]
        []
      , text "]"
      , button [ class "delete", onClick (Delete z) ] [ text "x" ]
      ]

expandControls z =
  let
    (t, bs) = z
  in
    [ div [ class "expandControls"]
      (case t of
        Tableau.Leaf n mc -> case mc of
          Nothing ->
            [ button [ onClick (ExpandAlpha z) ] [ text "α" ]
            , button [ onClick (ExpandBeta  z) ] [ text "β" ]
            , button [ onClick (MakeClosed  z) ] [ text "*" ]
            ]
          Just (r1,r2) ->
            let
              compl = Errors.errors <| areCloseRefsComplementary r1 r2 z
              ref1Cls = problemsClass <| (validateRef "Invalid close ref. #1" r1 z) ++ compl
              ref2Cls = problemsClass <| (validateRef "Invalid close ref. #1" r2 z) ++ compl
            in
              [ text "* "
              , input [ class ("refEdit " ++ ref1Cls), type_ "text"
                , placeholder "Ref", size 1, value r1.str, onInput <| SetClosed 0 z] []
              , input [ class ("refEdit " ++ ref2Cls), type_ "text"
                , placeholder "Ref", size 1, value r2.str, onInput <| SetClosed 1 z] []
              , button [ class "delete", onClick (MakeOpen z) ] [ text "x" ]
              ]
        Tableau.Alpha _ _ -> []
        Tableau.Beta _ _ _ -> []
      )
    ]

jsonDataUri json =
  "data:application/json;charset=utf-8," ++ Http.encodeUri json

jsonExportControl t =
  a
      [ type_ "button"
      , href <| jsonDataUri <| Tableau.Json.Encode.encode 2 t
      , downloadAs "tableau.json"
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
  case model.jsonImporting of
    True -> text "Loading file..."
    False ->
        label [ for model.jsonImportId ]
        [ button
          {- This is really ugly, but:
            - we really need the buton and onClick, if we want it to look like a button
              (embedding the label in a button or vice versa works in webkit but not in firefox)
            - Adding another Msg / Cmd just for this...
          -}
          [ attribute "onClick" ("javascript:document.getElementById('" ++ model.jsonImportId ++ "').click();") ]
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
    "" -> div [] []
    _ ->
      p
      [ class "jsonImportError" ]
      [ text <| "Error importing tableau: " ++ toString model.jsonImportError ]

{- vim: set sw=2 ts=2 sts=2 et : -}
