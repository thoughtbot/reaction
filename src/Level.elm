module Level exposing
    ( Level
    , Par(..)
    , buildLevel
    , levelHeight
    , levelObstacles
    , levelPar
    , levelWidth
    , map
    , parValue
    )

import Coordinates exposing (Height, Width)


type Par
    = Par Int


type Level o
    = Level
        { obstacles : o
        , width : Width
        , height : Height
        , par : Par
        }


parValue : Par -> Int
parValue (Par v) =
    v


buildLevel : Par -> Width -> Height -> o -> Level o
buildLevel p width height obstacles =
    Level
        { par = p
        , width = width
        , height = height
        , obstacles = obstacles
        }


map : (a -> b) -> Level a -> Level b
map f (Level l) =
    Level
        { par = l.par
        , width = l.width
        , height = l.height
        , obstacles = f l.obstacles
        }


levelObstacles : Level a -> a
levelObstacles (Level l) =
    l.obstacles


levelWidth : Level a -> Width
levelWidth (Level l) =
    l.width


levelHeight : Level a -> Height
levelHeight (Level l) =
    l.height


levelPar : Level a -> Par
levelPar (Level { par }) =
    par
