module View exposing (view)

import Browser
import Html exposing (..)
import Model exposing (Model(..), Msg(..))
import Page.Board.Main as Board
import Page.Home.Main as Home


view : Model -> Browser.Document Msg
view model =
    { title = "Reaction"
    , body =
        [ div []
            [ case model of
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
