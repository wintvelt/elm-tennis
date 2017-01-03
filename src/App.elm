module App exposing (..)

import Html exposing (Html, text, div, img)
import Html.Attributes exposing (src)


type alias Model =
    { message : String
    , logo : String
    }


init : ( Model, Cmd Msg )
init =
    ( { message = "Your Elm App is working!", logo = "" }, Cmd.none )


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ img [ src model.logo ] []
        , div [] [ text model.message ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
