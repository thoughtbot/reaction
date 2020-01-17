module Game exposing
    ( Board
    , BoardId
    , Game(..)
    , advanceBoard
    , advanceBoardId
    , boardClicks
    , boardIdUrlParser
    , clicksMade
    , completeGameWhenNoClustersRemain
    , gameBoard
    , getBoardId
    , incrementClicksOnCluster
    , isGameActive
    , loadBoards
    , mapBoard
    , parForBoard
    , renderableBoard
    , showBoardId
    )

import Coordinates exposing (Coordinates, Height, Width, dimensionsToCoordinates)
import Level exposing (Level, Par(..))
import LevelBuilder
import LevelParser
import List.Extra as List
import Obstacle exposing (Obstacle(..), handleObstacle, obstacleAtCoordinates)
import Particle exposing (..)
import Url.Parser as Parser exposing (Parser)


type ClickCounter
    = ClickCounter Int


type Game
    = Started Board
    | Complete Board Par ClickCounter


type BoardId
    = BoardId Int


type Board
    = Board
        { boardId : BoardId
        , clickCounter : ClickCounter
        , particles : Particles
        , obstacles : List Obstacle
        , level : Level (List Obstacle)
        }


showBoardId : BoardId -> String
showBoardId (BoardId bId) =
    String.fromInt bId


getBoardId : Board -> BoardId
getBoardId (Board { boardId }) =
    boardId


advanceBoardId : BoardId -> BoardId
advanceBoardId (BoardId bId) =
    BoardId <| bId + 1


parForBoard : Board -> Int
parForBoard (Board { level }) =
    Level.levelPar level
        |> Level.parValue


gameBoard : Game -> Board
gameBoard game =
    case game of
        Started board ->
            board

        Complete board _ _ ->
            board


isGameActive : Game -> Bool
isGameActive game =
    case game of
        Started _ ->
            True

        _ ->
            False


boardContainsClusters : Board -> Bool
boardContainsClusters (Board { obstacles }) =
    List.any Obstacle.isCluster obstacles


completeGameWhenNoClustersRemain : Game -> Game
completeGameWhenNoClustersRemain game =
    case game of
        Started ((Board { clickCounter, level }) as board) ->
            if boardContainsClusters board then
                Started board

            else
                Complete board (Level.levelPar level) clickCounter

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


boardClicks : Board -> Int
boardClicks (Board { clickCounter }) =
    clickCounterValue clickCounter


clicksMade : Game -> Int
clicksMade game =
    case game of
        Started board ->
            boardClicks board

        Complete _ _ (ClickCounter n) ->
            n


renderableBoard : Board -> List (List ( List Particle, Maybe Obstacle ))
renderableBoard ((Board { particles, obstacles }) as board) =
    let
        tileInformation coordinates =
            ( Particle.extract <| particlesAtCoordinates particles coordinates, obstacleAtCoordinates obstacles coordinates )

        allCoordinates =
            dimensionsToCoordinates (boardWidth board) (boardHeight board)
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


advanceParticles : Board -> Board
advanceParticles (Board ({ particles } as b)) =
    Board { b | particles = Particle.advanceParticles particles }


trimParticles : Board -> Board
trimParticles ((Board ({ particles } as b)) as board) =
    Board { b | particles = particlesWithinDimensions (boardWidth board) (boardHeight board) particles }


loadBoards : String -> List Board
loadBoards =
    List.indexedMap buildBoardFromLevel << loadLevels


loadLevels : String -> List (Level (List Obstacle))
loadLevels input =
    let
        parsedBoards =
            LevelParser.parseLevels input
    in
    Result.toMaybe parsedBoards
        |> Maybe.map (List.map LevelBuilder.parsedLevelToLevel)
        |> Maybe.withDefault []


buildBoardFromLevel : Int -> Level (List Obstacle) -> Board
buildBoardFromLevel boardId level =
    Board
        { boardId = BoardId <| boardId + 1
        , clickCounter = ClickCounter 0
        , particles = Particle.initial
        , obstacles = Level.levelObstacles level
        , level = level
        }


boardWidth : Board -> Width
boardWidth (Board { level }) =
    Level.levelWidth level


boardHeight : Board -> Height
boardHeight (Board { level }) =
    Level.levelHeight level


boardIdUrlParser : Parser (BoardId -> a) a
boardIdUrlParser =
    Parser.custom "BoardId" (Maybe.map BoardId << String.toInt)
