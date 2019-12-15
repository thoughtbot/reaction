module Obstacle exposing
    ( Obstacle(..)
    , Size(..)
    , buildSize
    , handleObstacle
    , incrementSize
    , obstacleAtCoordinates
    , showSize
    )

import Coordinates exposing (Coordinates)
import Direction exposing (Direction, allDirections, mirrorLeftDirection, mirrorRightDirection, reverseDirection, sidewaysDirections)
import Particle exposing (..)


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


handleObstacle : Obstacle -> Particles -> List Obstacle -> ( Particles, List Obstacle )
handleObstacle obstacle particles obstacles =
    case obstacle of
        Cluster (Size n) coordinates ->
            let
                excess =
                    Particle.length (particlesAtCoordinates particles coordinates) + n - 5
            in
            if excess >= 0 then
                ( particlesNotAtCoordinates particles coordinates
                    |> Particle.mappend (Particle.take excess (particlesAtCoordinates particles coordinates))
                    |> reactionAt coordinates
                , List.filter (\o -> o /= obstacle) obstacles
                )

            else
                let
                    newObstacle =
                        obstacle
                            |> increaseClusterSize (Particle.length <| particlesAtCoordinates particles coordinates)
                in
                ( particlesNotAtCoordinates particles coordinates
                , newObstacle :: List.filter (\o -> o /= obstacle) obstacles
                )

        Energizer coordinates ->
            let
                particleDirections =
                    List.map particleDirection <| Particle.extract <| particlesAtCoordinates particles coordinates

                potentiallyFireReaction ps =
                    if List.isEmpty particleDirections then
                        ps

                    else
                        List.foldl (energizeAt coordinates) ps particleDirections
            in
            ( particlesNotAtCoordinates particles coordinates
                |> potentiallyFireReaction
            , obstacles
            )

        BlackHole coordinates ->
            ( particlesNotAtCoordinates particles coordinates
            , obstacles
            )

        Mirror coordinates ->
            ( mapParticlesAtCoordinates (mapDirection reverseDirection) particles coordinates
            , obstacles
            )

        MirrorLeft coordinates ->
            ( mapParticlesAtCoordinates (mapDirection mirrorLeftDirection) particles coordinates
            , obstacles
            )

        MirrorRight coordinates ->
            ( mapParticlesAtCoordinates (mapDirection mirrorRightDirection) particles coordinates
            , obstacles
            )

        ChangeDirection newDirection coordinates ->
            ( mapParticlesAtCoordinates (mapDirection (always newDirection)) particles coordinates
            , obstacles
            )

        Portal coordinates1 coordinates2 ->
            let
                particlesAtCoordinates1 =
                    particlesAtCoordinates particles coordinates2
                        |> Particle.map (mapCoordinates (always coordinates1))

                particlesAtCoordinates2 =
                    particlesAtCoordinates particles coordinates1
                        |> Particle.map (mapCoordinates (always coordinates2))
            in
            ( particlesNotAtAnyCoordinates particles [ coordinates1, coordinates2 ]
                |> Particle.mappend particlesAtCoordinates1
                |> Particle.mappend particlesAtCoordinates2
            , obstacles
            )


reactionAt : Coordinates -> Particles -> Particles
reactionAt coordinates particles =
    List.foldl (buildParticle coordinates) particles allDirections


energizeAt : Coordinates -> Direction -> Particles -> Particles
energizeAt coordinates direction particles =
    List.foldl (buildParticle coordinates) particles (sidewaysDirections direction)


increaseClusterSize : Int -> Obstacle -> Obstacle
increaseClusterSize increasedSize obstacle =
    case obstacle of
        Cluster (Size n) coordinates ->
            Cluster (Size <| n + increasedSize) coordinates

        _ ->
            obstacle


showSize : Size -> String
showSize (Size n) =
    String.fromInt n


incrementSize : Size -> Size
incrementSize (Size n) =
    Size <| n + 1


buildSize : Int -> Size
buildSize =
    Size
