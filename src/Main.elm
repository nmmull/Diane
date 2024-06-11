module Main exposing (..)

import Browser
import Model exposing (initModel)
import Update exposing (noCmd, update, subscriptions)
import View exposing (view)

main =
  Browser.element
    { init = initModel >> noCmd
    , update = update
    , view = view
    , subscriptions = subscriptions
    }
