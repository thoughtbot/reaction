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
advanceBoard (Board particleId particles obstacles) =
    Board particleId (advanceParticles particles) obstacles


advanceParticles : List Particle -> List Particle
advanceParticles =
    List.map advanceParticle


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
    Board initialParticleId [] []
        |> createParticle Up (Coordinates (X 4) (Y 0))
        |> reactionAt (Coordinates (X 1) (Y 1))


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
