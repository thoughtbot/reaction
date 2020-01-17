module Session exposing
    ( Model
    , WithSession
    , initial
    , navKey
    )

import Browser.Navigation as Nav
import Game
import Levels


type alias WithSession a =
    { a | session : Model }


type alias Model =
    { key : Nav.Key
    , boards : List Game.Board
    }


initial : Nav.Key -> Model
initial key =
    { key = key
    , boards = Game.loadBoards Levels.levels
    }


navKey : Model -> Nav.Key
navKey { key } =
    key
