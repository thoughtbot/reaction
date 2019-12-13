module Main exposing (..)

import Browser
import Game exposing (Obstacle(..), Particle, Size(..))
import GameParser
import Html exposing (..)
import Html.Attributes exposing (class, classList, src)
import Html.Events exposing (onClick)
import Levels
import Time



---- MODEL ----


type alias Model =
    { game : Game.Game
    , boards : List Game.Board
    }


init : ( Model, Cmd Msg )
init =
    ( { game = Game.NotStarted
      , boards = Game.loadBoards Levels.levels
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | StartGame Game.Board
    | AdvanceBoard
    | ClickObstacle (Maybe Obstacle)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        StartGame board ->
            ( { model | game = Game.Started board }, Cmd.none )

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



---- VIEW ----


displayBoard : Int -> Game.Board -> Html Msg
displayBoard index board =
    div []
        [ button [ onClick <| StartGame board ] [ text <| "Start game " ++ String.fromInt index ]
        ]


view : Model -> Html Msg
view model =
    case model.game of
        Game.NotStarted ->
            div [] (List.indexedMap displayBoard model.boards)

        Game.Started board ->
            div []
                [ h2 [] [ text <| "Clicks: " ++ (String.fromInt <| Game.clicksMade model.game) ]
                , h2 [] [ text <| "Par: " ++ (String.fromInt <| Game.parForBoard board) ]
                , renderBoard <| Game.renderableBoard board
                ]

        Game.Complete board _ _ ->
            div []
                [ h2 [] [ text <| "Complete! Clicks: " ++ (String.fromInt <| Game.clicksMade model.game) ]
                , renderBoard <| Game.renderableBoard board
                ]


obstacleClass : Obstacle -> List String
obstacleClass obstacle =
    case obstacle of
        Cluster (Size n) _ ->
            [ "cluster", "cluster-" ++ String.fromInt n ]

        Portal _ _ ->
            [ "portal" ]

        Mirror _ ->
            [ "mirror" ]

        MirrorLeft _ ->
            [ "mirror-left" ]

        MirrorRight _ ->
            [ "mirror-right" ]

        ChangeDirection direction _ ->
            [ "change-direction", "change-direction-" ++ Game.showDirection direction ]

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
            span [ class <| "particle particle-" ++ (Game.showDirection <| Game.particleDirection particle) ] []
    in
    table [ class "board" ] (List.map renderRow boardTiles)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions =
            \{ game } ->
                if Game.isGameActive game then
                    Time.every 600 (always AdvanceBoard)

                else
                    Sub.none
        }


input : String
input =
    """
intro animation: None
par: 7
r b l l 3 l l w0
4 d d 2 d 4 u u 
r 2 r u d l l u 
r u r u d r 3 u 
2 l l u d d l u 
r r 3 u d r d u 
r r u u d 2 r d 
w0r r u 2 u r r 
"""
