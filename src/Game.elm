module Game exposing
    ( Board
    , Game(..)
    , Obstacle(..)
    , Particle
    , Size(..)
    , advanceBoard
    , clicksMade
    , completeGameWhenNoClustersRemain
    , incrementClicksOnCluster
    , initial
    , isGameActive
    , mapBoard
    , particleDirection
    , renderableBoard
    , showDirection
    )

import List.Extra as List


type ClickCounter
    = ClickCounter Int


type Game
    = NotStarted
    | Started Board
    | Complete Board ClickCounter


type Board
    = Board ParticleId ClickCounter Width Height (List Particle) (List Obstacle)


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
    | ChangeDirection Direction Coordinates
    | BlackHole Coordinates


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


isGameActive : Game -> Bool
isGameActive game =
    case game of
        Started _ ->
            True

        _ ->
            False


boardContainsClusters : Board -> Bool
boardContainsClusters (Board _ _ _ _ _ obstacles) =
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
        Started ((Board _ clickCounter _ _ _ _) as board) ->
            if boardContainsClusters board then
                Started board

            else
                Complete board clickCounter

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

        Started (Board _ (ClickCounter n) _ _ _ _) ->
            n

        Complete _ (ClickCounter n) ->
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
renderableBoard (Board _ _ (Width w) (Height h) particles obstacles) =
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
incrementClicksOnCluster coordinates ((Board particleId (ClickCounter clicks) width height particles obstacles) as board) =
    case obstacleAtCoordinates obstacles coordinates of
        Just ((Cluster (Size n) coords) as obstacle) ->
            let
                newObstacle =
                    Cluster (Size <| n + 1) coords
            in
            Board particleId
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

        ChangeDirection _ coords ->
            coords == coordinates

        BlackHole coords ->
            coords == coordinates


advanceBoard : Board -> Board
advanceBoard ((Board _ _ _ _ _ obstacles) as board) =
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


increaseClusterSize : Int -> Obstacle -> Obstacle
increaseClusterSize increasedSize obstacle =
    case obstacle of
        Cluster (Size n) coordinates ->
            Cluster (Size <| n + increasedSize) coordinates

        _ ->
            obstacle


handleObstacle : Obstacle -> Board -> Board
handleObstacle obstacle ((Board particleId clickCounter width height particles obstacles) as board) =
    case obstacle of
        Cluster (Size n) coordinates ->
            let
                excess =
                    List.length (particlesAtCoordinates particles coordinates) + n - 4
            in
            if excess >= 0 then
                Board
                    particleId
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
                    particleId
                    clickCounter
                    width
                    height
                    (particlesNotAtCoordinates particles coordinates)
                    (newObstacle :: List.filter (\o -> o /= obstacle) obstacles)

        BlackHole coordinates ->
            Board particleId clickCounter width height (particlesNotAtCoordinates particles coordinates) obstacles

        Mirror coordinates ->
            Board
                particleId
                clickCounter
                width
                height
                (mapParticlesAtCoordinates (mapDirection reverseDirection) particles coordinates)
                obstacles

        ChangeDirection newDirection coordinates ->
            Board
                particleId
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
                particleId
                clickCounter
                width
                height
                (particlesNotAtAnyCoordinates particles [ coordinates1, coordinates2 ] ++ particlesAtCoordinates1 ++ particlesAtCoordinates2)
                obstacles


advanceParticles : Board -> Board
advanceParticles (Board particleId clickCounter width height particles obstacles) =
    Board particleId clickCounter width height (List.map advanceParticle particles) obstacles


trimParticles : Board -> Board
trimParticles (Board particleId clickCounter width height particles obstacles) =
    let
        particleWithinBounds (Width w) (Height h) (Particle _ _ (Coordinates (X x) (Y y))) =
            List.member x (List.range 0 (w - 1)) && List.member y (List.range 0 (h - 1))
    in
    Board particleId clickCounter width height (List.filter (particleWithinBounds width height) particles) obstacles


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


initial : Board
initial =
    Board initialParticleId
        (ClickCounter 0)
        (Width 8)
        (Height 8)
        []
        [ Cluster (Size 3) (Coordinates (X 4) (Y 1))
        , Cluster (Size 3) (Coordinates (X 0) (Y 1))
        , Cluster (Size 1) (Coordinates (X 6) (Y 1))
        , Portal (Coordinates (X 0) (Y 3)) (Coordinates (X 6) (Y 3))
        , ChangeDirection Left (Coordinates (X 6) (Y 5))
        , BlackHole (Coordinates (X 4) (Y 3))
        ]
        |> createParticle Up (Coordinates (X 4) (Y 0))


reactionAt : Coordinates -> Board -> Board
reactionAt coordinates board =
    board
        |> createParticle Up coordinates
        |> createParticle Down coordinates
        |> createParticle Left coordinates
        |> createParticle Right coordinates


createParticle : Direction -> Coordinates -> Board -> Board
createParticle direction coordinates (Board (ParticleId particleId) clickCounter width height particles obstacles) =
    let
        newParticle =
            Particle (ParticleId particleId) direction coordinates
    in
    Board (ParticleId <| particleId + 1) clickCounter width height (newParticle :: particles) obstacles


initialParticleId : ParticleId
initialParticleId =
    ParticleId 1
