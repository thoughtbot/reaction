module Direction exposing
    ( Direction(..)
    , advanceCoordinatesInDirection
    , allDirections
    , mirrorLeftDirection
    , mirrorRightDirection
    , reverseDirection
    , showDirection
    , sidewaysDirections
    )

import Coordinates exposing (Coordinates, mapX, mapY)


type Direction
    = Up
    | Down
    | Left
    | Right


allDirections : List Direction
allDirections =
    [ Up, Right, Left, Down ]


showDirection : Direction -> String
showDirection direction =
    case direction of
        Up ->
            "up"

        Down ->
            "down"

        Left ->
            "left"

        Right ->
            "right"


reverseDirection : Direction -> Direction
reverseDirection direction =
    case direction of
        Up ->
            Down

        Down ->
            Up

        Left ->
            Right

        Right ->
            Left


mirrorLeftDirection : Direction -> Direction
mirrorLeftDirection direction =
    case direction of
        Up ->
            Left

        Down ->
            Right

        Left ->
            Up

        Right ->
            Down


mirrorRightDirection : Direction -> Direction
mirrorRightDirection direction =
    case direction of
        Up ->
            Right

        Down ->
            Left

        Left ->
            Down

        Right ->
            Up


sidewaysDirections : Direction -> List Direction
sidewaysDirections direction =
    case direction of
        Up ->
            [ Right, Up, Left ]

        Right ->
            [ Up, Right, Down ]

        Left ->
            [ Up, Left, Down ]

        Down ->
            [ Right, Left, Down ]


advanceCoordinatesInDirection : Direction -> Coordinates -> Coordinates
advanceCoordinatesInDirection direction coordinates =
    case direction of
        Up ->
            mapY ((+) 1) coordinates

        Down ->
            mapY (\y -> y - 1) coordinates

        Left ->
            mapX (\x -> x - 1) coordinates

        Right ->
            mapX ((+) 1) coordinates
