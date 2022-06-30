module Main exposing (main)

import Browser
import Editor exposing (init, update, view, subscriptions, Msg, Model)

main : Program (Maybe String) Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
