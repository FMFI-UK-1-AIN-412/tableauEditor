module Main exposing (main)

import Browser
import Json.Decode
import Editor exposing (init, update, view, subscriptions, Msg, Model)

main : Program (Maybe Json.Decode.Value) Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
