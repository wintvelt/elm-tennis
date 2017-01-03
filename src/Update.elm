module Update exposing (..)

import Model exposing (..)

{- Helper to award a point to a player in a normal set
Returns a SetResult:

 - If the set is still in progress: `SameSet SetInProgress`
 - If the set is over (won by the point winner): `SetWon { winner : Player, winScore : WinScore }`

 So if the set is won, the result can be handled by another function to update the Match
-}
gameFor : Player -> SetInProgress -> SetResult
gameFor player set =
    case set of
        UpToFive scores ->
            case player of
                Player1 ->
                    gameForHelper 
                        Player1 
                        (.player1) 
                        (\g s -> { s | s.player1 = g }) 
                        scores

                Player2 ->
                    gameForHelper 
                        Player2 
                        (.player2) 
                        (\g s -> { s | s.player2 = g }) 
                        scores

        SixToFive leader ->
            if player == leader then
                SetWon 
                    { winner = player 
                    , winScore = MaxFive Five
                    }
            else
                SameSet TieBreak

        TieBreak ->
            SetWon 
                { winner = player 
                , winScore = Six
                }

gameForHelper : 
    Player 
    -> (GameScores -> GamesUpToFive) 
    -> (GamesUpToFive -> GameScores -> GameScores) 
    -> GameScores 
    -> SetResult
gameForHelper player getScore updateScore scores =
    case getScore scores of
        Zero ->
            SameSet <| UpToFive <| updateScore One scores

        One ->
            SameSet <| UpToFive <| updateScore Two scores

        Two ->
            SameSet <| UpToFive <| updateScore Three scores

        Three ->
            SameSet <| UpToFive <| updateScore Four scores

        Four ->
            SameSet <| UpToFive <| updateScore Five scores

        Five ->
            SameSet <| SixToFive player
