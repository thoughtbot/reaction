module Model exposing
    ( Flags
    , Model
    , initial
    , nextBoard
    )

import Game
import Levels


type alias Flags =
    ()


type alias Model =
    { game : Game.Game
    , boards : List Game.Board
    }


initial : Model
initial =
    { game = Game.NotStarted
    , boards = Game.loadBoards Levels.levels
    }


currentBoard : Model -> Maybe Game.Board
currentBoard { game } =
    case game of
        Game.NotStarted ->
            Nothing

        Game.Started board ->
            Just board

        Game.Complete board _ _ ->
            Just board


nextBoard : Model -> Maybe Game.Board
nextBoard ({ boards } as model) =
    currentBoard model
        |> Maybe.andThen
            (\board ->
                List.filter (\b -> Game.getBoardId b == Game.advanceBoardId (Game.getBoardId board)) boards
                    |> List.head
            )
