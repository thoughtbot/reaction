module Page.Home.Main exposing
    ( Model
    , Msg(..)
    , init
    , subscriptions
    , update
    , view
    )

import Game
import Html exposing (..)
import Route
import Session exposing (WithSession)


type alias Model =
    WithSession {}


type Msg
    = NoOp


init : Session.Model -> ( Model, Cmd Msg )
init session =
    ( { session = session }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none


view : Model -> Html Msg
view { session } =
    div [] (List.map displayBoard session.boards)


displayBoard : Game.Board -> Html Msg
displayBoard board =
    let
        boardId =
            Game.getBoardId board
    in
    div []
        [ a [ Route.href <| Route.ShowBoard boardId ]
            [ text <| "Start level " ++ Game.showBoardId boardId
            ]
        ]
