module MainEmbeddable exposing (main)

import Browser
import Json.Decode
import Editor exposing (init, update, viewEmbeddable, subscriptions, Msg, Model)

main : Program (Maybe Json.Decode.Value) Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = viewEmbeddable
        , subscriptions = subscriptions
        }
