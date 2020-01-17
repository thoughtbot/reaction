module Page.Board.Main exposing
    ( Model
    , Msg(..)
    , init
    , subscriptions
    , update
    , view
    )

import Direction
import Game
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Obstacle exposing (Obstacle(..))
import Particle exposing (Particle)
import Route
import Session exposing (WithSession)
import Time


type alias Model =
    WithSession { game : Game.Game }


type Msg
    = ClickObstacle (Maybe Obstacle)
    | AdvanceBoard


init : Session.Model -> Game.Board -> ( Model, Cmd Msg )
init session board =
    ( { session = session, game = Game.Started board }, Cmd.none )


nextBoard : Model -> Maybe Game.Board
nextBoard model =
    let
        boards =
            model.session.boards

        currentBoard =
            Game.gameBoard model.game
    in
    List.filter (\b -> Game.getBoardId b == Game.advanceBoardId (Game.getBoardId currentBoard)) boards
        |> List.head


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 600 (always AdvanceBoard)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickObstacle (Just (Cluster _ coordinates)) ->
            ( { model
                | game =
                    Game.mapBoard (Game.incrementClicksOnCluster coordinates) model.game
              }
            , Cmd.none
            )

        ClickObstacle _ ->
            ( model, Cmd.none )

        AdvanceBoard ->
            let
                newGame =
                    Game.mapBoard Game.advanceBoard model.game
                        |> Game.completeGameWhenNoClustersRemain
            in
            ( { model | game = newGame }, Cmd.none )


view : Model -> Html Msg
view ({ game } as model) =
    case game of
        Game.Started board ->
            div []
                [ h2 [] [ text <| "Clicks: " ++ (String.fromInt <| Game.clicksMade game) ]
                , h2 [] [ text <| "Par: " ++ (String.fromInt <| Game.parForBoard board) ]
                , endGameLink
                , renderBoard <| Game.renderableBoard board
                ]

        Game.Complete board _ _ ->
            div []
                [ h2 [] [ text <| "Complete! Clicks: " ++ (String.fromInt <| Game.clicksMade game) ]
                , endGameLink
                , nextBoardButton model
                , renderBoard <| Game.renderableBoard board
                ]


endGameLink : Html a
endGameLink =
    a [ Route.href Route.Home ] [ text "End game" ]


nextBoardButton : Model -> Html Msg
nextBoardButton model =
    case nextBoard model of
        Just nextBoard_ ->
            a [ Route.href <| Route.ShowBoard <| Game.getBoardId nextBoard_ ] [ text "Go on to next board" ]

        Nothing ->
            text ""


obstacleClass : Obstacle -> List String
obstacleClass obstacle =
    case obstacle of
        Cluster size _ ->
            [ "cluster", "cluster-" ++ Obstacle.showSize size ]

        Portal _ _ ->
            [ "portal" ]

        Mirror _ ->
            [ "mirror" ]

        MirrorLeft _ ->
            [ "mirror-left" ]

        MirrorRight _ ->
            [ "mirror-right" ]

        ChangeDirection direction _ ->
            [ "change-direction", "change-direction-" ++ Direction.showDirection direction ]

        BlackHole _ ->
            [ "black-hole" ]

        Energizer _ ->
            [ "energizer" ]


renderBoard : List (List ( List Particle, Maybe Obstacle )) -> Html Msg
renderBoard boardTiles =
    let
        renderRow columns =
            tr [] (List.map renderColumn columns)

        classes particles obstacle =
            (Maybe.withDefault [] <| Maybe.map (\o -> List.map (\s -> ( s, True )) <| obstacleClass o) obstacle) ++ [ ( "has-particle", List.length particles > 0 ) ]

        renderColumn ( particles, obstacle ) =
            td [ classList <| classes particles obstacle, onClick <| ClickObstacle obstacle ]
                [ span [] (List.map showParticle particles) ]

        showParticle particle =
            span [ class <| "particle particle-" ++ (Direction.showDirection <| Particle.particleDirection particle) ] []
    in
    table [ class "board" ] (List.map renderRow boardTiles)
