module Player exposing (..)


type Player
    = Player1
    | Player2


defaultName : Player -> String
defaultName player =
    case player of
        Player1 ->
            "Player 1"

        Player2 ->
            "Player 2"


otherPlayer : Player -> Player
otherPlayer player =
    case player of
        Player1 ->
            Player2

        Player2 ->
            Player1
