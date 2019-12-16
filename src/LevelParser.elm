module LevelParser exposing
    ( ParsedObstacle(..)
    , isPortal
    , parseLevels
    )

import Coordinates exposing (buildHeight, buildWidth)
import Direction exposing (Direction(..))
import Level exposing (Level, Par(..))
import Parser exposing (..)


type ParsedObstacle
    = Cluster Int
    | ChangeDirection Direction
    | Portal Int
    | BlackHole
    | Energizer
    | Mirror
    | MirrorLeft
    | MirrorRight
    | Empty


type alias List2D a =
    List (List a)


isPortal : ParsedObstacle -> Bool
isPortal obstacle =
    case obstacle of
        Portal _ ->
            True

        _ ->
            False


parseLevels : String -> Result String (List (Level (List2D ParsedObstacle)))
parseLevels input =
    run levelsParser input
        |> Result.mapError deadEndsToString


levelsParser : Parser (List (Level (List2D ParsedObstacle)))
levelsParser =
    succeed identity
        |. newlineParser
        |= many levelParser


levelParser : Parser (Level (List2D ParsedObstacle))
levelParser =
    succeed buildLevel
        |. animationParser
        |= parParser
        |= rowsParser


buildLevel : Par -> List2D ParsedObstacle -> Level (List2D ParsedObstacle)
buildLevel par rows =
    let
        height =
            List.length rows

        width =
            List.head rows |> Maybe.withDefault [] |> List.length
    in
    Level.buildLevel par (buildWidth width) (buildHeight height) rows


animationParser : Parser ()
animationParser =
    succeed ()
        |. token "intro animation: "
        |. chompWhile (\c -> Char.isAlphaNum c || c == ' ')
        |. newlineParser


rowsParser : Parser (List2D ParsedObstacle)
rowsParser =
    sepBy rowParser newlineParser


rowParser : Parser (List ParsedObstacle)
rowParser =
    many obstacleParser


obstacleParser : Parser ParsedObstacle
obstacleParser =
    oneOf
        [ succeed Empty
            |. token "- "
        , succeed Energizer
            |. token "e "
        , succeed Cluster
            |= int
            |. chompIf (\c -> c == ' ')
        , succeed (ChangeDirection Down)
            |. token "d "
        , succeed (ChangeDirection Up)
            |. token "u "
        , succeed (ChangeDirection Right)
            |. token "r "
        , succeed (ChangeDirection Left)
            |. token "l "
        , succeed BlackHole
            |. token "b "
        , succeed MirrorLeft
            |. token "mL"
        , succeed MirrorRight
            |. token "mR"
        , succeed Mirror
            |. token "m "
        , succeed Portal
            |. token "w"
            |= singleCharacterInt
        ]


singleCharacterInt : Parser Int
singleCharacterInt =
    succeed ()
        |. chompIf Char.isDigit
        |> getChompedString
        |> andThen
            (\d ->
                String.toInt d
                    |> Maybe.map succeed
                    |> Maybe.withDefault (problem "unable to parse")
            )


newlineParser : Parser ()
newlineParser =
    succeed ()
        |. chompIf (\c -> c == '\n')


parParser : Parser Par
parParser =
    succeed Par
        |. token "par: "
        |= int
        |. newlineParser


sepBy : Parser a -> Parser b -> Parser (List a)
sepBy parser separator =
    let
        f acc =
            oneOf
                [ succeed (\a -> Loop (a :: acc))
                    |= parser
                    |. separator
                , succeed ()
                    |> map (always <| Done (List.reverse acc))
                ]
    in
    loop [] f


many : Parser a -> Parser (List a)
many parser =
    let
        f acc =
            oneOf
                [ succeed (\a -> Loop (a :: acc))
                    |= parser
                , succeed ()
                    |> map (always <| Done (List.reverse acc))
                ]
    in
    loop [] f
