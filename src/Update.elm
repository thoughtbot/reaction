module Update exposing
    ( Msg(..)
    , init
    , subscriptions
    , update
    )

import Game
import Model exposing (Model)
import Obstacle exposing (Obstacle(..))
import Time


type Msg
    = NoOp
    | StartGame Game.Board
    | EndGame
    | AdvanceBoard
    | ClickObstacle (Maybe Obstacle)


init : Model.Flags -> ( Model, Cmd Msg )
init _ =
    ( Model.initial, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        StartGame board ->
            ( { model | game = Game.Started board }, Cmd.none )

        EndGame ->
            ( { model | game = Game.NotStarted }, Cmd.none )

        AdvanceBoard ->
            ( { model
                | game =
                    Game.mapBoard Game.advanceBoard model.game
                        |> Game.completeGameWhenNoClustersRemain
              }
            , Cmd.none
            )

        ClickObstacle (Just (Cluster _ coordinates)) ->
            ( { model | game = Game.mapBoard (Game.incrementClicksOnCluster coordinates) model.game }, Cmd.none )

        ClickObstacle _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions { game } =
    if Game.isGameActive game then
        Time.every 600 (always AdvanceBoard)

    else
        Sub.none
