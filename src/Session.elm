module Session exposing
    ( GameSpeed(..)
    , Model
    , WithSession
    , initial
    , navKey
    )

import Browser.Navigation as Nav
import Game
import Levels


type GameSpeed
    = Normal
    | Fast
    | Faster


type alias WithSession a =
    { a | session : Model }


type alias Model =
    { key : Nav.Key
    , boards : List Game.Board
    , gameSpeed : GameSpeed
    }


initial : Nav.Key -> Model
initial key =
    { key = key
    , boards = Game.loadBoards Levels.levels
    , gameSpeed = Normal
    }


navKey : Model -> Nav.Key
navKey { key } =
    key
