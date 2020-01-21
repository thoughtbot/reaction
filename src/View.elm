module View exposing (view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (value)
import Html.Events exposing (on, targetValue)
import Json.Decode
import Model exposing (Model(..), Msg(..))
import Page.Board.Main as Board
import Page.Home.Main as Home
import Session


view : Model -> Browser.Document Msg
view model =
    { title = "Reaction"
    , body =
        [ div []
            [ gameSpeedDropdown model
            , case model of
                Home homeModel ->
                    Html.map HandleHomeMsg <| Home.view homeModel

                Board boardModel ->
                    Html.map HandleBoardMsg <| Board.view boardModel

                NotFound _ ->
                    text "not found"

                Loading _ ->
                    text "loading"
            ]
        ]
    }


gameSpeedDropdown : Model -> Html Msg
gameSpeedDropdown model =
    let
        generateOption speed =
            option
                [ value <| showSpeed speed
                ]
                [ text <| showSpeed speed ]
    in
    select
        [ on "change" (Json.Decode.map SetGameSpeed gameSpeedDecoder) ]
        (List.map generateOption [ Session.Normal, Session.Fast, Session.Faster ])


showSpeed : Session.GameSpeed -> String
showSpeed gameSpeed =
    case gameSpeed of
        Session.Normal ->
            "Normal"

        Session.Fast ->
            "Fast"

        Session.Faster ->
            "Faster"


gameSpeedDecoder : Json.Decode.Decoder Session.GameSpeed
gameSpeedDecoder =
    targetValue
        |> Json.Decode.andThen
            (\v ->
                case v of
                    "Normal" ->
                        Json.Decode.succeed Session.Normal

                    "Fast" ->
                        Json.Decode.succeed Session.Fast

                    "Faster" ->
                        Json.Decode.succeed Session.Faster

                    _ ->
                        Json.Decode.fail "Unknown speed"
            )
