module Main exposing (..)

import Browser
import Game
import Html exposing (..)
import Html.Attributes exposing (src)



---- MODEL ----


type alias Model =
    Game.Board


init : ( Model, Cmd Msg )
init =
    ( Game.initial, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ text <| Debug.toString model
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
