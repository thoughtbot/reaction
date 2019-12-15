module Particle exposing
    ( Particle
    , ParticleId
    , Particles
    , advanceParticles
    , buildParticle
    , extract
    , initial
    , length
    , map
    , mapCoordinates
    , mapDirection
    , mapParticlesAtCoordinates
    , mappend
    , particleCoordinates
    , particleDirection
    , particlesAtCoordinates
    , particlesNotAtAnyCoordinates
    , particlesNotAtCoordinates
    , particlesWithinDimensions
    , take
    )

import Coordinates exposing (Coordinates, Height, Width, coordinatesWithinDimensions)
import Direction exposing (Direction, advanceCoordinatesInDirection)


type ParticleId
    = ParticleId Int


type Particle
    = Particle ParticleId Direction Coordinates


type Particles
    = Particles
        { particleId : ParticleId
        , particles : List Particle
        }


map : (Particle -> Particle) -> Particles -> Particles
map f (Particles ({ particles } as p)) =
    Particles { p | particles = List.map f particles }


extract : Particles -> List Particle
extract (Particles { particles }) =
    particles


take : Int -> Particles -> Particles
take n =
    flatMap (List.take n)


mappend : Particles -> Particles -> Particles
mappend (Particles p1) (Particles ({ particles } as p)) =
    Particles { p | particles = particles ++ p1.particles }


length : Particles -> Int
length =
    List.length << extract


advanceParticle : Particle -> Particle
advanceParticle ((Particle _ direction _) as particle) =
    mapCoordinates (advanceCoordinatesInDirection direction) particle


buildParticle : Coordinates -> Direction -> Particles -> Particles
buildParticle coordinates direction (Particles { particleId, particles }) =
    let
        newParticle =
            Particle particleId direction coordinates
    in
    Particles
        { particleId = incrementParticleId particleId
        , particles = newParticle :: particles
        }


incrementParticleId : ParticleId -> ParticleId
incrementParticleId (ParticleId value) =
    ParticleId <| value + 1


initial : Particles
initial =
    Particles
        { particleId = initialParticleId
        , particles = []
        }


initialParticleId : ParticleId
initialParticleId =
    ParticleId 1


particleCoordinates : Particle -> Coordinates
particleCoordinates (Particle _ _ coordinates) =
    coordinates


particleDirection : Particle -> Direction
particleDirection (Particle _ direction _) =
    direction


particleAtCoordinates : Coordinates -> Particle -> Bool
particleAtCoordinates coords (Particle _ _ particleCoords) =
    coords == particleCoords


particlesAtCoordinates : Particles -> Coordinates -> Particles
particlesAtCoordinates particles coordinates =
    particles
        |> flatMap (List.filter (particleAtCoordinates coordinates))


particlesNotAtCoordinates : Particles -> Coordinates -> Particles
particlesNotAtCoordinates particles coordinates =
    particles
        |> flatMap (List.filter (not << particleAtCoordinates coordinates))


particlesNotAtAnyCoordinates : Particles -> List Coordinates -> Particles
particlesNotAtAnyCoordinates particles coordinatesList =
    particles
        |> flatMap
            (List.filter
                (\(Particle _ _ coordinates) ->
                    not <| List.member coordinates coordinatesList
                )
            )


mapParticlesAtCoordinates : (Particle -> Particle) -> Particles -> Coordinates -> Particles
mapParticlesAtCoordinates f particles coordinates =
    particles
        |> map
            (\particle ->
                if particleAtCoordinates coordinates particle then
                    f particle

                else
                    particle
            )


mapCoordinates : (Coordinates -> Coordinates) -> Particle -> Particle
mapCoordinates f (Particle particleId direction coordinates) =
    Particle particleId direction (f coordinates)


mapDirection : (Direction -> Direction) -> Particle -> Particle
mapDirection f (Particle particleId direction coordinates) =
    Particle particleId (f direction) coordinates


advanceParticles : Particles -> Particles
advanceParticles =
    flatMap (List.map advanceParticle)


particlesWithinDimensions : Width -> Height -> Particles -> Particles
particlesWithinDimensions width height =
    flatMap (List.filter (coordinatesWithinDimensions width height << particleCoordinates))


flatMap : (List Particle -> List Particle) -> Particles -> Particles
flatMap f (Particles p) =
    Particles { p | particles = f p.particles }
