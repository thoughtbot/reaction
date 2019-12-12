module Game exposing (Board, advanceBoard, initial)


type Board
    = Board


advanceBoard : Board -> Board
advanceBoard =
    identity


initial : Board
initial =
    Board
