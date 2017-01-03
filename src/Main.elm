module Main exposing (..)

import App exposing (..)
import Html exposing (program)


main : Program String Model Msg
main =
    program { view = view, init = init, update = update, subscriptions = subscriptions }
