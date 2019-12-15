module Particle exposing
    ( Particle
    , ParticleId
    , advanceParticles
    , buildParticle
    , incrementParticleId
    , initialParticleId
    , mapCoordinates
    , mapDirection
    , mapParticlesAtCoordinates
    , particleCoordinates
    , particleDirection
    , particlesAtCoordinates
    , particlesNotAtAnyCoordinates
    , particlesNotAtCoordinates
    , particlesWithinDimensions
    )

import Coordinates exposing (Coordinates, Height, Width, coordinatesWithinDimensions)
import Direction exposing (Direction, advanceCoordinatesInDirection)


type ParticleId
    = ParticleId Int


type Particle
    = Particle ParticleId Direction Coordinates


advanceParticle : Particle -> Particle
advanceParticle (Particle particleId direction coordinates) =
    Particle particleId direction (advanceCoordinatesInDirection direction coordinates)


buildParticle : ParticleId -> Direction -> Coordinates -> Particle
buildParticle =
    Particle


incrementParticleId : ParticleId -> ParticleId
incrementParticleId (ParticleId value) =
    ParticleId <| value + 1


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


advanceParticles : List Particle -> List Particle
advanceParticles =
    List.map advanceParticle


particlesWithinDimensions : Width -> Height -> List Particle -> List Particle
particlesWithinDimensions width height =
    List.filter (coordinatesWithinDimensions width height << particleCoordinates)
