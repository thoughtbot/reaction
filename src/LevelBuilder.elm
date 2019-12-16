module LevelBuilder exposing (parsedLevelToLevel)

import Coordinates exposing (Coordinates, coordinatesFromPair)
import Level exposing (Level)
import LevelParser exposing (ParsedObstacle(..))
import List.Extra as List
import Obstacle exposing (Obstacle(..))


parsedLevelToLevel : Level (List (List ParsedObstacle)) -> Level (List Obstacle)
parsedLevelToLevel =
    Level.map flattenAndParseObstacles


flattenAndParseObstacles : List (List ParsedObstacle) -> List Obstacle
flattenAndParseObstacles rows =
    catMaybes <|
        parseObstacles <|
            List.concat <|
                List.indexedMap
                    (\y row ->
                        List.indexedMap
                            (\x col ->
                                ( coordinatesFromPair ( x, y ), col )
                            )
                            row
                    )
                <|
                    List.reverse rows


parseObstacles : List ( Coordinates, ParsedObstacle ) -> List (Maybe Obstacle)
parseObstacles allObstacles =
    let
        nonPortals =
            List.filter
                (not << LevelParser.isPortal << Tuple.second)
                allObstacles

        portals =
            List.filter
                (LevelParser.isPortal << Tuple.second)
                allObstacles

        samePortals o1 o2 =
            case ( o1, o2 ) of
                ( LevelParser.Portal n1, LevelParser.Portal n2 ) ->
                    n1 == n2

                _ ->
                    False

        portalObstacles =
            List.gatherWith (\( _, o1 ) ( _, o2 ) -> samePortals o1 o2) portals
                |> List.map
                    (\( o, os ) ->
                        case ( o, os ) of
                            ( ( c1, _ ), [ ( c2, _ ) ] ) ->
                                Just <| Obstacle.Portal c1 c2

                            _ ->
                                Nothing
                    )
    in
    List.map (uncurry parseObstacle) nonPortals ++ portalObstacles


parseObstacle : Coordinates -> ParsedObstacle -> Maybe Obstacle
parseObstacle coordinates parsedObstacle =
    case parsedObstacle of
        LevelParser.Empty ->
            Nothing

        LevelParser.Cluster size ->
            Just <| Obstacle.Cluster (Obstacle.buildSize size) coordinates

        LevelParser.ChangeDirection direction ->
            Just <| Obstacle.ChangeDirection direction coordinates

        LevelParser.BlackHole ->
            Just <| Obstacle.BlackHole coordinates

        LevelParser.Energizer ->
            Just <| Obstacle.Energizer coordinates

        LevelParser.Mirror ->
            Just <| Obstacle.Mirror coordinates

        LevelParser.MirrorLeft ->
            Just <| Obstacle.MirrorLeft coordinates

        LevelParser.MirrorRight ->
            Just <| Obstacle.MirrorRight coordinates

        LevelParser.Portal _ ->
            Nothing


catMaybes : List (Maybe a) -> List a
catMaybes =
    List.filterMap identity


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b
