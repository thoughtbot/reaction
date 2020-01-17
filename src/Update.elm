module Update exposing
    ( init
    , subscriptions
    , update
    )

import Browser
import Browser.Navigation as Nav
import Model exposing (Model(..), Msg(..))
import Obstacle exposing (Obstacle(..))
import Page.Board.Main as Board
import Page.Home.Main as Home
import Route
import Session
import Url exposing (Url)


init : Model.Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        session =
            Session.initial key
    in
    Model.changeRouteTo (Route.fromUrl url)
        (Loading session)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            ( model, Cmd.none )

                        Just _ ->
                            ( model
                            , Nav.pushUrl (Session.navKey (Model.toSession model)) (Url.toString url)
                            )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( ChangedUrl url, _ ) ->
            Model.changeRouteTo (Route.fromUrl url) model

        ( HandleHomeMsg subMsg, Home model_ ) ->
            let
                ( newModel, newMsg ) =
                    Home.update subMsg model_
            in
            ( Home newModel
            , Cmd.map HandleHomeMsg newMsg
            )

        ( HandleBoardMsg subMsg, Board model_ ) ->
            let
                ( newModel, newMsg ) =
                    Board.update subMsg model_
            in
            ( Board newModel
            , Cmd.map HandleBoardMsg newMsg
            )

        ( _, _ ) ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Home homeModel ->
            Sub.map HandleHomeMsg <| Home.subscriptions homeModel

        Board boardModel ->
            Sub.map HandleBoardMsg <| Board.subscriptions boardModel

        _ ->
            Sub.none
