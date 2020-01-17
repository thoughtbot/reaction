module View exposing (view)

import Direction
import Game
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Model exposing (Model)
import Obstacle exposing (Obstacle(..))
import Particle exposing (Particle)
import Update exposing (Msg(..))


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
                , endGameButton
                , renderBoard <| Game.renderableBoard board
                ]

        Game.Complete board _ _ ->
            div []
                [ h2 [] [ text <| "Complete! Clicks: " ++ (String.fromInt <| Game.clicksMade model.game) ]
                , endGameButton
                , nextBoardButton model
                , renderBoard <| Game.renderableBoard board
                ]


endGameButton : Html Msg
endGameButton =
    button [ onClick EndGame ] [ text "End game" ]


nextBoardButton : Model -> Html Msg
nextBoardButton model =
    case Model.nextBoard model of
        Just nextBoard_ ->
            button [ onClick <| StartGame nextBoard_ ] [ text "Go on to next board" ]

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
