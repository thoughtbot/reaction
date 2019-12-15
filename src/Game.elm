module Game exposing
    ( Board
    , Game(..)
    , Obstacle(..)
    , Size(..)
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


type Size
    = Size Int


type Obstacle
    = Cluster Size Coordinates
    | Portal Coordinates Coordinates
    | Mirror Coordinates
    | MirrorLeft Coordinates
    | MirrorRight Coordinates
    | ChangeDirection Direction Coordinates
    | BlackHole Coordinates
    | Energizer Coordinates


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
        Just ((Cluster (Size n) coords) as obstacle) ->
            let
                newObstacle =
                    Cluster (Size <| n + 1) coords
            in
            Board
                { b
                    | clickCounter = incrementClickCounter clickCounter
                    , obstacles =
                        newObstacle :: List.filter (\o -> o /= obstacle) obstacles
                }

        _ ->
            board


obstacleAtCoordinates : List Obstacle -> Coordinates -> Maybe Obstacle
obstacleAtCoordinates obstacles coordinates =
    List.filter (singleObstacleAtCoordinates coordinates) obstacles
        |> List.head


singleObstacleAtCoordinates : Coordinates -> Obstacle -> Bool
singleObstacleAtCoordinates coordinates obstacle =
    case obstacle of
        Cluster _ coords ->
            coords == coordinates

        Portal coords1 coords2 ->
            coords1 == coordinates || coords2 == coordinates

        Mirror coords ->
            coords == coordinates

        MirrorLeft coords ->
            coords == coordinates

        MirrorRight coords ->
            coords == coordinates

        ChangeDirection _ coords ->
            coords == coordinates

        BlackHole coords ->
            coords == coordinates

        Energizer coords ->
            coords == coordinates


advanceBoard : Board -> Board
advanceBoard ((Board { obstacles }) as board) =
    List.foldl handleObstacle board obstacles
        |> advanceParticles
        |> trimParticles



-- Board particleId (advanceParticles particles) obstacles
-- for each obstacle, check if there's overlap
-- if there is, do whatever we need to do to each particle overlapping with the obstacle
--   and also do whatever we need to do with the obstacle


increaseClusterSize : Int -> Obstacle -> Obstacle
increaseClusterSize increasedSize obstacle =
    case obstacle of
        Cluster (Size n) coordinates ->
            Cluster (Size <| n + increasedSize) coordinates

        _ ->
            obstacle


handleObstacle : Obstacle -> Board -> Board
handleObstacle obstacle ((Board ({ particles, obstacles } as b)) as board) =
    case obstacle of
        Cluster (Size n) coordinates ->
            let
                excess =
                    Particle.length (particlesAtCoordinates particles coordinates) + n - 5
            in
            if excess >= 0 then
                Board
                    { b
                        | particles =
                            particlesNotAtCoordinates particles coordinates
                                |> Particle.mappend (Particle.take excess (particlesAtCoordinates particles coordinates))
                        , obstacles =
                            List.filter (\o -> o /= obstacle) obstacles
                    }
                    |> reactionAt coordinates

            else
                let
                    newObstacle =
                        obstacle
                            |> increaseClusterSize (Particle.length <| particlesAtCoordinates particles coordinates)
                in
                Board
                    { b
                        | particles =
                            particlesNotAtCoordinates particles coordinates
                        , obstacles =
                            newObstacle :: List.filter (\o -> o /= obstacle) obstacles
                    }

        Energizer coordinates ->
            let
                particleDirections =
                    List.map particleDirection <| Particle.extract <| particlesAtCoordinates particles coordinates

                potentiallyFireReaction board_ =
                    if List.isEmpty particleDirections then
                        board_

                    else
                        List.foldl (\d b_ -> energizeAt coordinates d b_) board_ particleDirections
            in
            Board
                { b
                    | particles =
                        particlesNotAtCoordinates particles coordinates
                }
                |> potentiallyFireReaction

        BlackHole coordinates ->
            Board { b | particles = particlesNotAtCoordinates particles coordinates }

        Mirror coordinates ->
            Board
                { b
                    | particles =
                        mapParticlesAtCoordinates (mapDirection reverseDirection) particles coordinates
                }

        MirrorLeft coordinates ->
            Board
                { b
                    | particles =
                        mapParticlesAtCoordinates (mapDirection mirrorLeftDirection) particles coordinates
                }

        MirrorRight coordinates ->
            Board
                { b
                    | particles =
                        mapParticlesAtCoordinates (mapDirection mirrorRightDirection) particles coordinates
                }

        ChangeDirection newDirection coordinates ->
            Board
                { b
                    | particles =
                        mapParticlesAtCoordinates (mapDirection (always newDirection)) particles coordinates
                }

        Portal coordinates1 coordinates2 ->
            let
                particlesAtCoordinates1 =
                    particlesAtCoordinates particles coordinates2
                        |> Particle.map (mapCoordinates (always coordinates1))

                particlesAtCoordinates2 =
                    particlesAtCoordinates particles coordinates1
                        |> Particle.map (mapCoordinates (always coordinates2))
            in
            Board
                { b
                    | particles =
                        particlesNotAtAnyCoordinates particles [ coordinates1, coordinates2 ]
                            |> Particle.mappend particlesAtCoordinates1
                            |> Particle.mappend particlesAtCoordinates2
                }


advanceParticles : Board -> Board
advanceParticles (Board ({ particles } as b)) =
    Board { b | particles = Particle.advanceParticles particles }


trimParticles : Board -> Board
trimParticles (Board ({ width, height, particles } as b)) =
    Board { b | particles = particlesWithinDimensions width height particles }


reactionAt : Coordinates -> Board -> Board
reactionAt coordinates board =
    List.foldl (createParticle coordinates) board allDirections


energizeAt : Coordinates -> Direction -> Board -> Board
energizeAt coordinates direction board =
    List.foldl (\d b -> createParticle coordinates d b) board (sidewaysDirections direction)


createParticle : Coordinates -> Direction -> Board -> Board
createParticle coordinates direction (Board ({ particles } as b)) =
    Board
        { b
            | particles = buildParticle direction coordinates particles
        }


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
            Just <| Cluster (Size size) coordinates

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
