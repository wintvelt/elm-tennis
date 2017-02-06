module Set exposing (..)

{-

   type Set(..)
   type SetOn
   type SetScore
   type SetResult(..)
   newSetWithGame : a -> SetWithGame a
   pointInSet : Player -> SetWithGame a -> SetResult a

   newSet : SetOn

   gameFor : Player -> SetOn -> Set
   getSetScores : Player -> SetScore -> ( String, String )
   getSetOnScores : SetOn -> ( String, String )

      In a set, each won game is a point,
      A set is by the first player to win 6 games,
      with a lead of at least 2 games.
      At a score of 6-5, another game is played.
      If the leader wins again, the final score is 7-5.

      If not, and the score is 6-6, then
      in a normal set, the next game will lead to
      a deciding score of 7-6 for one player

      in the last set (5th set in a best-of-five match)
      players will continue to play until
      one of the players has a lead of 2 games
      (in the longest game in history, the score in
      the last set was 70-68)
-}

import Player exposing (..)


type GamesToFive
    = Zero
    | One
    | Two
    | Three
    | Four
    | Five


type SetOn
    = SetStart GamesToFive GamesToFive
    | SixFive Player
    | SixAll



-- helpers to award a game to a player (only normal sets)


type Set
    = SameSet SetOn
    | SetWonBy SetScore


type SetScore
    = VsGames GamesToFive
    | VsSix


newSet : SetOn
newSet =
    SetStart Zero Zero


gameFor : Player -> SetOn -> Set
gameFor player set =
    case set of
        SetStart Five Five ->
            SixFive player
                |> SameSet

        SetStart pl1games Five ->
            case player of
                Player1 ->
                    SetStart (addGame pl1games) Five
                        |> SameSet

                Player2 ->
                    SetWonBy (VsGames pl1games)

        SetStart Five pl2games ->
            case player of
                Player1 ->
                    SetWonBy Player1 (VsGames pl2games)

                Player2 ->
                    SetStart Five (addGame pl2games)
                        |> SameSet

        SetStart pl1games pl2games ->
            case player of
                Player1 ->
                    SetStart (addGame pl1games) pl2games
                        |> SameSet

                Player2 ->
                    SetStart pl1games (addGame pl1games)
                        |> SameSet

        SixFive leader ->
            if leader == player then
                SetWonBy (VsGames Five)
            else
                SixAll
                    |> SameSet

        SixAll ->
            SetWonBy VsSix



-- helper to increase games won by 1


addGame : GamesToFive -> GamesToFive
addGame games =
    case games of
        Zero ->
            One

        One ->
            Two

        Two ->
            Three

        Three ->
            Four

        Four ->
            Five

        Five ->
            -- theoretical case
            Five



-- helpers for match


type alias SetWithGame a =
    { a
        | currentGame : GameOn
        , currentSet : SetOn
    }


type SetResult a
    = SameSetWithGame (SetWithGame a)
    | SetWon SetScore


newSetWithGame : a -> SetWithGame a
newSetWithGame a =
    { a
        | currentGame = newGame
        , currentSet = newSet
    }


pointInSet : Player -> SetWithGame a -> SetResult a
pointInSet player set =
    case pointInGame player set.currentGame of
        SameGame gameOn ->
            { set | currentGame = gameOn }
                |> SameSetWithGame

        GameWon ->
            case gameInSet player set.currentSet of
                SameSet setOn ->
                    { set
                        | currentGame = newGame
                        , currentSet = setOn
                    }
                        |> SameSetWithGame

                SetWonBy setScore ->
                    SetWon setScore



-- helpers to get a string values for the score


getSetScores : Player -> SetScore -> ( String, String )
getSetScores player games =
    case ( player, games ) of
        ( Player1, VsSix ) ->
            ( "7", "6" )

        ( Player1, VsGames Five ) ->
            ( "7", "5" )

        ( Player1, VsGames pl2games ) ->
            ( "6", setString pl2games )

        ( Player2, VsSix ) ->
            ( "6", "7" )

        ( Player2, VsGames Five ) ->
            ( "5", "7" )

        ( Player2, VsGames pl1games ) ->
            ( setString pl1games, "6" )


getSetOnScores : SetOn -> ( String, String )
getSetOnScores setOn =
    case setOn of
        SetStart pl1games pl2games ->
            ( setString pl1games, setString pl2games )

        SixFive Player1 ->
            ( "6", "5" )

        SixFive Player2 ->
            ( "5", "6" )

        SixAll ->
            ( "6", "6" )


setString : GamesToFive -> String
setString games =
    case games of
        Zero ->
            "0"

        One ->
            "1"

        Two ->
            "2"

        Three ->
            "3"

        Four ->
            "4"

        Five ->
            "5"
