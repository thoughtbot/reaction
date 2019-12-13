module Game exposing
    ( Board
    , Game(..)
    , Obstacle(..)
    , Particle
    , Size(..)
    , advanceBoard
    , clicksMade
    , completeGameWhenNoClustersRemain
    , getBoardId
    , incrementClicksOnCluster
    , isGameActive
    , loadBoards
    , mapBoard
    , parForBoard
    , particleDirection
    , renderableBoard
    , showDirection
    )

import GameParser exposing (ParsedBoard(..))
import List.Extra as List


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
    = Board BoardId ParticleId Par ClickCounter Width Height (List Particle) (List Obstacle)


type Width
    = Width Int


type Height
    = Height Int


type ParticleId
    = ParticleId Int


type Particle
    = Particle ParticleId Direction Coordinates


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


type Direction
    = Up
    | Down
    | Left
    | Right


type Coordinates
    = Coordinates X Y


type X
    = X Int


type Y
    = Y Int


getBoardId : Board -> BoardId
getBoardId (Board boardId _ _ _ _ _ _ _) =
    boardId


parForBoard : Board -> Int
parForBoard (Board _ _ (Par n) _ _ _ _ _) =
    n


registerObstacle : Obstacle -> Board -> Board
registerObstacle obstacle (Board boardId particleId par clickCounter width height particles obstacles) =
    Board
        boardId
        particleId
        par
        clickCounter
        width
        height
        particles
        (obstacle :: obstacles)


isGameActive : Game -> Bool
isGameActive game =
    case game of
        Started _ ->
            True

        _ ->
            False


boardContainsClusters : Board -> Bool
boardContainsClusters (Board _ _ _ _ _ _ _ obstacles) =
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
        Started ((Board _ _ par clickCounter _ _ _ _) as board) ->
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


clicksMade : Game -> Int
clicksMade game =
    case game of
        NotStarted ->
            0

        Started (Board _ _ _ (ClickCounter n) _ _ _ _) ->
            n

        Complete _ _ (ClickCounter n) ->
            n


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


particleDirection : Particle -> Direction
particleDirection (Particle _ direction _) =
    direction


coordinatesFromPair : ( Int, Int ) -> Coordinates
coordinatesFromPair ( x, y ) =
    Coordinates (X x) (Y y)


renderableBoard : Board -> List (List ( List Particle, Maybe Obstacle ))
renderableBoard (Board _ _ _ _ (Width w) (Height h) particles obstacles) =
    let
        tileInformation coordinates =
            ( particlesAtCoordinates particles coordinates, obstacleAtCoordinates obstacles coordinates )

        xs =
            List.range 0 (w - 1)

        ys =
            List.range 0 (h - 1)

        allCoordinates =
            List.reverse <| List.map (\y -> List.map (\x -> coordinatesFromPair ( x, y )) xs) ys
    in
    List.map (List.map tileInformation) allCoordinates


incrementClicksOnCluster : Coordinates -> Board -> Board
incrementClicksOnCluster coordinates ((Board boardId particleId par (ClickCounter clicks) width height particles obstacles) as board) =
    case obstacleAtCoordinates obstacles coordinates of
        Just ((Cluster (Size n) coords) as obstacle) ->
            let
                newObstacle =
                    Cluster (Size <| n + 1) coords
            in
            Board boardId
                particleId
                par
                (ClickCounter <| clicks + 1)
                width
                height
                particles
                (newObstacle :: List.filter (\o -> o /= obstacle) obstacles)

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
advanceBoard ((Board _ _ _ _ _ _ _ obstacles) as board) =
    List.foldl handleObstacle board obstacles
        |> advanceParticles
        |> trimParticles



-- Board particleId (advanceParticles particles) obstacles
-- for each obstacle, check if there's overlap
-- if there is, do whatever we need to do to each particle overlapping with the obstacle
--   and also do whatever we need to do with the obstacle


particleAtCoordinates : Coordinates -> Particle -> Bool
particleAtCoordinates coords (Particle _ _ particleCoords) =
    coords == particleCoords


particlesAtCoordinates : List Particle -> Coordinates -> List Particle
particlesAtCoordinates particles coordinates =
    List.filter (particleAtCoordinates coordinates) particles


particlesNotAtCoordinates : List Particle -> Coordinates -> List Particle
particlesNotAtCoordinates particles coordinates =
    List.filter (not << particleAtCoordinates coordinates) particles


particlesNotAtAnyCoordinates : List Particle -> List Coordinates -> List Particle
particlesNotAtAnyCoordinates particles coordinatesList =
    List.filter
        (\(Particle _ _ coordinates) ->
            not <| List.member coordinates coordinatesList
        )
        particles


mapParticlesAtCoordinates : (Particle -> Particle) -> List Particle -> Coordinates -> List Particle
mapParticlesAtCoordinates f particles coordinates =
    List.map
        (\particle ->
            if particleAtCoordinates coordinates particle then
                f particle

            else
                particle
        )
        particles


mapCoordinates : (Coordinates -> Coordinates) -> Particle -> Particle
mapCoordinates f (Particle particleId direction coordinates) =
    Particle particleId direction (f coordinates)


mapDirection : (Direction -> Direction) -> Particle -> Particle
mapDirection f (Particle particleId direction coordinates) =
    Particle particleId (f direction) coordinates


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


increaseClusterSize : Int -> Obstacle -> Obstacle
increaseClusterSize increasedSize obstacle =
    case obstacle of
        Cluster (Size n) coordinates ->
            Cluster (Size <| n + increasedSize) coordinates

        _ ->
            obstacle


handleObstacle : Obstacle -> Board -> Board
handleObstacle obstacle ((Board boardId particleId par clickCounter width height particles obstacles) as board) =
    case obstacle of
        Cluster (Size n) coordinates ->
            let
                excess =
                    List.length (particlesAtCoordinates particles coordinates) + n - 5
            in
            if excess >= 0 then
                Board
                    boardId
                    particleId
                    par
                    clickCounter
                    width
                    height
                    (particlesNotAtCoordinates particles coordinates ++ List.take excess (particlesAtCoordinates particles coordinates))
                    (List.filter (\o -> o /= obstacle) obstacles)
                    |> reactionAt coordinates

            else
                let
                    newObstacle =
                        obstacle
                            |> increaseClusterSize (List.length <| particlesAtCoordinates particles coordinates)
                in
                Board
                    boardId
                    particleId
                    par
                    clickCounter
                    width
                    height
                    (particlesNotAtCoordinates particles coordinates)
                    (newObstacle :: List.filter (\o -> o /= obstacle) obstacles)

        Energizer coordinates ->
            let
                particleDirections =
                    List.map particleDirection <| particlesAtCoordinates particles coordinates

                potentiallyFireReaction board_ =
                    if List.isEmpty particleDirections then
                        board_

                    else
                        List.foldl (\d b -> energizeAt coordinates d b) board_ particleDirections
            in
            Board
                boardId
                particleId
                par
                clickCounter
                width
                height
                (particlesNotAtCoordinates particles coordinates)
                obstacles
                |> potentiallyFireReaction

        BlackHole coordinates ->
            Board boardId particleId par clickCounter width height (particlesNotAtCoordinates particles coordinates) obstacles

        Mirror coordinates ->
            Board
                boardId
                particleId
                par
                clickCounter
                width
                height
                (mapParticlesAtCoordinates (mapDirection reverseDirection) particles coordinates)
                obstacles

        MirrorLeft coordinates ->
            Board
                boardId
                particleId
                par
                clickCounter
                width
                height
                (mapParticlesAtCoordinates (mapDirection mirrorLeftDirection) particles coordinates)
                obstacles

        MirrorRight coordinates ->
            Board
                boardId
                particleId
                par
                clickCounter
                width
                height
                (mapParticlesAtCoordinates (mapDirection mirrorRightDirection) particles coordinates)
                obstacles

        ChangeDirection newDirection coordinates ->
            Board
                boardId
                particleId
                par
                clickCounter
                width
                height
                (mapParticlesAtCoordinates (mapDirection (always newDirection)) particles coordinates)
                obstacles

        Portal coordinates1 coordinates2 ->
            let
                particlesAtCoordinates1 =
                    particlesAtCoordinates particles coordinates2
                        |> List.map (mapCoordinates (always coordinates1))

                particlesAtCoordinates2 =
                    particlesAtCoordinates particles coordinates1
                        |> List.map (mapCoordinates (always coordinates2))
            in
            Board
                boardId
                particleId
                par
                clickCounter
                width
                height
                (particlesNotAtAnyCoordinates particles [ coordinates1, coordinates2 ] ++ particlesAtCoordinates1 ++ particlesAtCoordinates2)
                obstacles


advanceParticles : Board -> Board
advanceParticles (Board boardId particleId par clickCounter width height particles obstacles) =
    Board boardId particleId par clickCounter width height (List.map advanceParticle particles) obstacles


trimParticles : Board -> Board
trimParticles (Board boardId particleId par clickCounter width height particles obstacles) =
    let
        particleWithinBounds (Width w) (Height h) (Particle _ _ (Coordinates (X x) (Y y))) =
            List.member x (List.range 0 (w - 1)) && List.member y (List.range 0 (h - 1))
    in
    Board boardId particleId par clickCounter width height (List.filter (particleWithinBounds width height) particles) obstacles


advanceParticle : Particle -> Particle
advanceParticle (Particle particleId direction (Coordinates (X x) (Y y))) =
    case direction of
        Up ->
            Particle particleId direction (Coordinates (X x) (Y <| y + 1))

        Down ->
            Particle particleId direction (Coordinates (X x) (Y <| y - 1))

        Left ->
            Particle particleId direction (Coordinates (X <| x - 1) (Y y))

        Right ->
            Particle particleId direction (Coordinates (X <| x + 1) (Y y))


reactionAt : Coordinates -> Board -> Board
reactionAt coordinates board =
    board
        |> createParticle Up coordinates
        |> createParticle Down coordinates
        |> createParticle Left coordinates
        |> createParticle Right coordinates


energizeAt : Coordinates -> Direction -> Board -> Board
energizeAt coordinates direction board =
    let
        otherDirections =
            case direction of
                Up ->
                    [ Right, Up, Left ]

                Right ->
                    [ Up, Right, Down ]

                Left ->
                    [ Up, Left, Down ]

                Down ->
                    [ Right, Left, Down ]
    in
    List.foldl (\d b -> createParticle d coordinates b) board otherDirections


createParticle : Direction -> Coordinates -> Board -> Board
createParticle direction coordinates (Board boardId (ParticleId particleId) par clickCounter width height particles obstacles) =
    let
        newParticle =
            Particle (ParticleId particleId) direction coordinates
    in
    Board boardId (ParticleId <| particleId + 1) par clickCounter width height (newParticle :: particles) obstacles


initialParticleId : ParticleId
initialParticleId =
    ParticleId 1


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

        GameParser.ChangeDirection GameParser.Up ->
            Just <| ChangeDirection Up coordinates

        GameParser.ChangeDirection GameParser.Down ->
            Just <| ChangeDirection Down coordinates

        GameParser.ChangeDirection GameParser.Left ->
            Just <| ChangeDirection Left coordinates

        GameParser.ChangeDirection GameParser.Right ->
            Just <| ChangeDirection Right coordinates

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
    Board (BoardId boardId)
        initialParticleId
        (Par par)
        (ClickCounter 0)
        (Width width)
        (Height height)
        []
        []
        |> (\board -> List.foldl (\o b -> registerObstacle o b) board obstaclesToAdd)
