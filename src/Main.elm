module Main exposing (..)

import Browser
import Game exposing (Obstacle(..), Particle, Size(..))
import Html exposing (..)
import Html.Attributes exposing (class, classList, src)
import Html.Events exposing (onClick)
import Time



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
    | ClickObstacle (Maybe Obstacle)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AdvanceBoard ->
            ( Game.advanceBoard model, Cmd.none )

        ClickObstacle (Just (Cluster _ coordinates)) ->
            ( Game.incrementClicksOnCluster coordinates model, Cmd.none )

        ClickObstacle _ ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text <| "Clicks: " ++ (String.fromInt <| Game.clicksMade model) ]
        , renderBoard <| Game.renderableBoard model
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

        ChangeDirection direction _ ->
            [ "change-direction", "change-direction-" ++ Game.showDirection direction ]

        BlackHole _ ->
            [ "black-hole" ]


renderBoard : List (List ( List Particle, Maybe Obstacle )) -> Html Msg
renderBoard boardTiles =
    let
        renderRow columns =
            tr [] (List.map renderColumn columns)

        classes particles obstacle =
            (Maybe.withDefault [] <| Maybe.map (\o -> List.map (\s -> ( s, True )) <| obstacleClass o) obstacle) ++ [ ( "has-particle", List.length particles > 0 ) ]

        renderColumn ( particles, obstacle ) =
            td [ classList <| classes particles obstacle, onClick <| ClickObstacle obstacle ]
                (List.map showParticle particles)

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
        , subscriptions = \_ -> Time.every 600 (always AdvanceBoard)
        }
