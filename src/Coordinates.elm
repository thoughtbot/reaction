module Coordinates exposing
    ( Coordinates
    , Height
    , Width
    , buildHeight
    , buildWidth
    , coordinatesFromPair
    , coordinatesWithinDimensions
    , dimensionsToCoordinates
    , mapX
    , mapY
    )


type Width
    = Width Int


type Height
    = Height Int


type X
    = X Int


type Y
    = Y Int


type Coordinates
    = Coordinates X Y


buildWidth : Int -> Width
buildWidth =
    Width


buildHeight : Int -> Height
buildHeight =
    Height


coordinatesFromPair : ( Int, Int ) -> Coordinates
coordinatesFromPair ( x, y ) =
    Coordinates (X x) (Y y)


mapX : (Int -> Int) -> Coordinates -> Coordinates
mapX f (Coordinates (X x) y) =
    Coordinates (X <| f x) y


mapY : (Int -> Int) -> Coordinates -> Coordinates
mapY f (Coordinates x (Y y)) =
    Coordinates x (Y <| f y)


dimensionsToCoordinates : Width -> Height -> List (List Coordinates)
dimensionsToCoordinates (Width w) (Height h) =
    let
        xs =
            List.range 0 (w - 1)

        ys =
            List.range 0 (h - 1)
    in
    List.reverse <| List.map (\y -> List.map (\x -> coordinatesFromPair ( x, y )) xs) ys


coordinatesWithinDimensions : Width -> Height -> Coordinates -> Bool
coordinatesWithinDimensions (Width w) (Height h) (Coordinates (X x) (Y y)) =
    List.member x (List.range 0 (w - 1)) && List.member y (List.range 0 (h - 1))
