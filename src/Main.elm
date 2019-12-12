module Main exposing (..)

import Browser
import Game
import Html exposing (..)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)



---- MODEL ----


type alias Model =
    Game.Board


init : ( Model, Cmd Msg )
init =
    ( Game.initial, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | AdvanceBoard


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AdvanceBoard ->
            ( Game.advanceBoard model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ text <| Debug.toString model
        , button [ onClick AdvanceBoard ] [ text "Advance" ]
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
