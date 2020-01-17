module Model exposing
    ( Flags
    , Model(..)
    , Msg(..)
    , changeRouteTo
    , toSession
    )

import Browser
import Game
import Page.Board.Main as Board
import Page.Home.Main as Home
import Route exposing (Route)
import Session
import Url exposing (Url)


type alias Flags =
    ()


type Model
    = Home Home.Model
    | NotFound Session.Model
    | Loading Session.Model
    | Board Board.Model


type Msg
    = HandleHomeMsg Home.Msg
    | HandleBoardMsg Board.Msg
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest


toSession : Model -> Session.Model
toSession model =
    case model of
        Home { session } ->
            session

        NotFound session ->
            session

        Loading session ->
            session

        Board { session } ->
            session


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.Home ->
            Home.init session
                |> updateWith Home HandleHomeMsg

        Just (Route.ShowBoard boardId) ->
            case findBoard session.boards boardId of
                Nothing ->
                    ( NotFound session, Cmd.none )

                Just board ->
                    Board.init session board
                        |> updateWith Board HandleBoardMsg


findBoard : List Game.Board -> Game.BoardId -> Maybe Game.Board
findBoard boards boardId =
    List.filter (\b -> Game.getBoardId b == boardId) boards
        |> List.head


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )
