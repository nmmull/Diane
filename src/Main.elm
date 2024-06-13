module Main exposing (..)

import Browser
import Diane exposing (Prog)
import Model exposing (Model, initModel)
import Update exposing (Msg, Flags, noCmd, update, subscriptions)
import View exposing (view)

init : Flags -> ( Model, Cmd Msg)
init flags = noCmd (initModel flags)

main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
