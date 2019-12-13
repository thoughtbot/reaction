module GameParser exposing
    ( Par(..)
    , ParsedBoard(..)
    , ParsedDirection(..)
    , ParsedObstacle(..)
    , parseBoard
    )

import Parser exposing (..)


type Par
    = Par Int


type ParsedBoard
    = ParsedBoard Par (List (List ParsedObstacle))


type ParsedObstacle
    = Cluster Int
    | ChangeDirection ParsedDirection
    | Portal Int
    | BlackHole
    | Energizer
    | Mirror
    | MirrorLeft
    | MirrorRight
    | Empty


type ParsedDirection
    = Up
    | Right
    | Down
    | Left


parseBoard : String -> Result String ParsedBoard
parseBoard input =
    run boardParser input
        |> Result.mapError deadEndsToString


boardParser : Parser ParsedBoard
boardParser =
    succeed ParsedBoard
        |. newlineParser
        |. animationParser
        |= parParser
        |= rowsParser


animationParser : Parser ()
animationParser =
    succeed ()
        |. token "intro animation: "
        |. chompWhile (\c -> Char.isAlphaNum c || c == ' ')
        |. newlineParser


rowsParser : Parser (List (List ParsedObstacle))
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
