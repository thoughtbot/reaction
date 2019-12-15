module Game exposing
    ( Board
    , Game(..)
    , advanceBoard
    , advanceBoardId
    , clicksMade
    , completeGameWhenNoClustersRemain
    , getBoardId
    , incrementClicksOnCluster
    , isGameActive
    , loadBoards
    , mapBoard
    , parForBoard
    , renderableBoard
    )

import Coordinates exposing (..)
import Direction exposing (..)
import GameParser exposing (ParsedBoard(..))
import List.Extra as List
import Obstacle exposing (..)
import Particle exposing (..)


type ClickCounter
    = ClickCounter Int


type Par
    = Par Int


type Game
    = NotStarted
    | Started Board
    | Complete Board Par ClickCounter


type BoardId
    = BoardId Int


type Board
    = Board
        { boardId : BoardId
        , par : Par
        , clickCounter : ClickCounter
        , width : Width
        , height : Height
        , particles : Particles
        , obstacles : List Obstacle
        }


getBoardId : Board -> BoardId
getBoardId (Board { boardId }) =
    boardId


advanceBoardId : BoardId -> BoardId
advanceBoardId (BoardId bId) =
    BoardId <| bId + 1


parForBoard : Board -> Int
parForBoard (Board { par }) =
    let
        (Par n) =
            par
    in
    n


registerObstacle : Obstacle -> Board -> Board
registerObstacle obstacle (Board ({ obstacles } as b)) =
    Board { b | obstacles = obstacle :: obstacles }


isGameActive : Game -> Bool
isGameActive game =
    case game of
        Started _ ->
            True

        _ ->
            False


boardContainsClusters : Board -> Bool
boardContainsClusters (Board { obstacles }) =
    List.any
        (\o ->
            case o of
                Cluster _ _ ->
                    True

                _ ->
                    False
        )
        obstacles


completeGameWhenNoClustersRemain : Game -> Game
completeGameWhenNoClustersRemain game =
    case game of
        Started ((Board { par, clickCounter }) as board) ->
            if boardContainsClusters board then
                Started board

            else
                Complete board par clickCounter

        _ ->
            game


mapBoard : (Board -> Board) -> Game -> Game
mapBoard f game =
    case game of
        Started board ->
            Started <| f board

        _ ->
            game


clickCounterValue : ClickCounter -> Int
clickCounterValue (ClickCounter value) =
    value


incrementClickCounter : ClickCounter -> ClickCounter
incrementClickCounter (ClickCounter value) =
    ClickCounter <| value + 1


clicksMade : Game -> Int
clicksMade game =
    case game of
        NotStarted ->
            0

        Started (Board { clickCounter }) ->
            clickCounterValue clickCounter

        Complete _ _ (ClickCounter n) ->
            n


renderableBoard : Board -> List (List ( List Particle, Maybe Obstacle ))
renderableBoard (Board { width, height, particles, obstacles }) =
    let
        tileInformation coordinates =
            ( Particle.extract <| particlesAtCoordinates particles coordinates, obstacleAtCoordinates obstacles coordinates )

        allCoordinates =
            dimensionsToCoordinates width height
    in
    List.map (List.map tileInformation) allCoordinates


incrementClicksOnCluster : Coordinates -> Board -> Board
incrementClicksOnCluster coordinates ((Board ({ obstacles, clickCounter } as b)) as board) =
    case obstacleAtCoordinates obstacles coordinates of
        Just ((Cluster size coords) as obstacle) ->
            let
                newObstacle =
                    Cluster (Obstacle.incrementSize size) coords
            in
            Board
                { b
                    | clickCounter = incrementClickCounter clickCounter
                    , obstacles =
                        newObstacle :: List.filter (\o -> o /= obstacle) obstacles
                }

        _ ->
            board


advanceBoard : Board -> Board
advanceBoard ((Board bo) as board) =
    let
        handleObstacleOnBoard obstacle (Board ({ particles, obstacles } as b)) =
            let
                outcome =
                    handleObstacle obstacle particles obstacles
            in
            Board
                { b
                    | particles = Tuple.first <| outcome
                    , obstacles = Tuple.second <| outcome
                }
    in
    List.foldl handleObstacleOnBoard board bo.obstacles
        |> advanceParticles
        |> trimParticles



-- Board particleId (advanceParticles particles) obstacles
-- for each obstacle, check if there's overlap
-- if there is, do whatever we need to do to each particle overlapping with the obstacle
--   and also do whatever we need to do with the obstacle


advanceParticles : Board -> Board
advanceParticles (Board ({ particles } as b)) =
    Board { b | particles = Particle.advanceParticles particles }


trimParticles : Board -> Board
trimParticles (Board ({ width, height, particles } as b)) =
    Board { b | particles = particlesWithinDimensions width height particles }


loadBoards : String -> List Board
loadBoards input =
    let
        parsedBoards =
            GameParser.parseBoards input
    in
    Result.toMaybe parsedBoards
        |> Maybe.map (List.indexedMap parsedBoardToBoard)
        |> Maybe.withDefault []


parseObstacles : List ( Coordinates, GameParser.ParsedObstacle ) -> List (Maybe Obstacle)
parseObstacles allObstacles =
    let
        nonPortals =
            List.filter
                (\( _, o ) -> not <| isPortal o)
                allObstacles

        portals =
            List.filter
                (\( _, o ) -> isPortal o)
                allObstacles

        isPortal o =
            case o of
                GameParser.Portal _ ->
                    True

                _ ->
                    False

        samePortals o1 o2 =
            case ( o1, o2 ) of
                ( GameParser.Portal n1, GameParser.Portal n2 ) ->
                    n1 == n2

                _ ->
                    False

        portalObstacles =
            List.gatherWith (\( _, o1 ) ( _, o2 ) -> samePortals o1 o2) portals
                |> List.map
                    (\( o, os ) ->
                        case ( o, os ) of
                            ( ( c1, _ ), [ ( c2, _ ) ] ) ->
                                Just <| Portal c1 c2

                            _ ->
                                Nothing
                    )
    in
    List.map (\( c, o ) -> parseObstacle c o) nonPortals ++ portalObstacles


parseObstacle : Coordinates -> GameParser.ParsedObstacle -> Maybe Obstacle
parseObstacle coordinates parsedObstacle =
    case parsedObstacle of
        GameParser.Empty ->
            Nothing

        GameParser.Cluster size ->
            Just <| Cluster (Obstacle.buildSize size) coordinates

        GameParser.ChangeDirection direction ->
            Just <| ChangeDirection direction coordinates

        GameParser.BlackHole ->
            Just <| BlackHole coordinates

        GameParser.Energizer ->
            Just <| Energizer coordinates

        GameParser.Mirror ->
            Just <| Mirror coordinates

        GameParser.MirrorLeft ->
            Just <| MirrorLeft coordinates

        GameParser.MirrorRight ->
            Just <| MirrorRight coordinates

        GameParser.Portal id ->
            Nothing


parsedBoardToBoard : Int -> GameParser.ParsedBoard -> Board
parsedBoardToBoard boardId parsedBoard =
    let
        (ParsedBoard (GameParser.Par par) rows) =
            parsedBoard

        width =
            List.head cleanedRows |> Maybe.withDefault [] |> List.length

        height =
            List.length cleanedRows

        cleanedRows =
            List.filter (not << List.isEmpty) rows
                |> List.reverse

        obstaclesToAdd =
            List.filterMap identity <|
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
                            cleanedRows
    in
    Board
        { boardId = BoardId boardId
        , par = Par par
        , clickCounter = ClickCounter 0
        , width = buildWidth width
        , height = buildHeight height
        , particles = Particle.initial
        , obstacles = []
        }
        |> (\board -> List.foldl (\o b -> registerObstacle o b) board obstaclesToAdd)
