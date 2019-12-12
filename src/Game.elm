module Game exposing (Board, advanceBoard, initial)


type Board
    = Board ParticleId (List Particle) (List Obstacle)


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


advanceBoard : Board -> Board
advanceBoard ((Board _ _ obstacles) as board) =
    List.foldl handleObstacle board obstacles
        |> advanceParticles



-- Board particleId (advanceParticles particles) obstacles
-- for each obstacle, check if there's overlap
-- if there is, do whatever we need to do to each particle overlapping with the obstacle
--   and also do whatever we need to do with the obstacle


particlesAtCoordinates : List Particle -> Coordinates -> List Particle
particlesAtCoordinates particles coordinates =
    let
        particleAtCoordinates coords (Particle _ _ particleCoords) =
            coords == particleCoords
    in
    List.filter (particleAtCoordinates coordinates) particles


particlesNotAtCoordinates : List Particle -> Coordinates -> List Particle
particlesNotAtCoordinates particles coordinates =
    let
        particleAtCoordinates coords (Particle _ _ particleCoords) =
            coords /= particleCoords
    in
    List.filter (particleAtCoordinates coordinates) particles


handleObstacle : Obstacle -> Board -> Board
handleObstacle obstacle ((Board particleId particles obstacles) as board) =
    case obstacle of
        Cluster (Size n) coordinates ->
            let
                excess =
                    List.length (particlesAtCoordinates particles coordinates) + n - 4
            in
            if excess >= 0 then
                Board
                    particleId
                    (particlesNotAtCoordinates particles coordinates ++ List.take excess (particlesAtCoordinates particles coordinates))
                    (List.filter (\o -> o /= obstacle) obstacles)
                    |> reactionAt coordinates

            else
                board

        BlackHole coordinates ->
            Board particleId (particlesNotAtCoordinates particles coordinates) obstacles

        _ ->
            board


advanceParticles : Board -> Board
advanceParticles (Board particleId particles obstacles) =
    Board particleId (List.map advanceParticle particles) obstacles


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
        []
        [ Cluster (Size 3) (Coordinates (X 4) (Y 1))
        , Cluster (Size 3) (Coordinates (X 0) (Y 1))
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
createParticle direction coordinates (Board (ParticleId particleId) particles obstacles) =
    let
        newParticle =
            Particle (ParticleId particleId) direction coordinates
    in
    Board (ParticleId <| particleId + 1) (newParticle :: particles) obstacles


initialParticleId : ParticleId
initialParticleId =
    ParticleId 1
