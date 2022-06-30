module MainEmbeddable exposing (main)

import Browser
import Editor exposing (init, update, viewEmbeddable, subscriptions, Msg, Model)

main : Program (Maybe String) Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = viewEmbeddable
        , subscriptions = subscriptions
        }
