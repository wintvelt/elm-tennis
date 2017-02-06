module LastSet exposing (..)

{-

   type LastSetOn
   type LastSetScore
   type LastSetResult(..)
   newLastSet : LastSetOn

   gameInLastSet : Player -> LastSetOn -> LastSetResult
   getLastSetScores :  LastSetScore -> ( String, String )
   getLastSetOnScores :  LastSetOn -> ( String, String )

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


type LastSet
    = SameLastSet LastSetOn
    | LastSetWonBy LastSetScore


type LastSetOn
    = LastSetStart GamesToFive GamesToFive
    | SixFive Player
    | InRound WinRound


type Round
    = NoGames
    | LeadBy Player
    | WithNext Round


type RoundResult
    = SameRound Round
    | RoundWon WinRound


type LastSetScore
    = VsGames GamesToFive
    | VsRounds WinRound


type WinRound
    = LastRound
    | Another WinRound



-- helpers to award a game to a player (last set specifically)


type LastSetResult
    = SameLastSet LastSetOn
    | LastSetWon LastSetScore


newLastSet : LastSetOn
newLastSet =
    LastSetStart Zero Zero


gameInLastSet : Player -> LastSetOn -> LastSetResult
gameInLastSet player lastSetOn =
    case lastSetOn of
        LastSetStart Five Five ->
            SixFive player
                |> SameLastSet

        LastSetStart pl1games Five ->
            case player of
                Player1 ->
                    SetStart (addGame pl1games) Five
                        |> SameLastSet

                Player2 ->
                    VsGames pl1games
                        |> LastSetWon

        LastSetStart Five pl2games ->
            case player of
                Player1 ->
                    VsGames pl2games
                        |> LastSetWon

                Player2 ->
                    LastSetStart Five (addGame pl2games)
                        |> SameLastSet

        LastSetStart pl1games pl2games ->
            case player of
                Player1 ->
                    LastSetStart (addGame pl1games) pl2games
                        |> SameLastSet

                Player2 ->
                    LastSetStart pl1games (addGame pl1games)
                        |> SameLastSet

        SixFive leader ->
            if leader == player then
                VsGames Five
                    |> LastSetWon
            else
                InRound NoGames
                    |> SameLastSet

        InRound rnd ->
            case gameInRoundFor player rnd of
                SameRound newRound ->
                    InRound newRound
                        |> SameLastSet

                RoundWon winRound ->
                    VsRounds winRound
                        |> LastSetWon


gameInRoundFor : Player -> Round -> RoundResult
gameInRoundFor player rnd =
    case rnd of
        NoGames ->
            LeadBy player
                |> SameRound

        LeadBy leader ->
            if player == leader then
                LastRound
                    |> RoundWon
            else
                WithNext NoGames
                    |> SameRound

        WithNext nextRound ->
            case gameInRoundFor nextRound of
                SameRound newNextRound ->
                    WithNext newNextRound
                        |> SameRound

                RoundWon winRound ->
                    Another winRound
                        |> RoundWon



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



-- helpers to get a string values for the score


getLastSetScores : Player -> LastSetScore -> ( String, String )
getLastSetScores winner games =
    case ( winner, games ) of
        ( Player1, VsGames Five ) ->
            ( "7", "5" )

        ( Player1, VsGames pl2games ) ->
            ( "6", setString pl2games )

        ( Player2, VsGames Five ) ->
            ( "5", "7" )

        ( Player2, VsGames pl1games ) ->
            ( setString pl1games, "6" )

        ( player, VsRounds winRound ) ->
            let
                games =
                    getRounds winRound
            in
                case player of
                    Player1 ->
                        ( toString <| games + 6 + 2
                        , toString <| games + 6
                        )

                    Player2 ->
                        ( toString <| games + 6
                        , toString <| games + 6 + 2
                        )


getRounds : WinRound -> Int
getRounds winRound =
    case winRound of
        LastRound ->
            0

        Another nextRound ->
            1 + getRounds nextRound


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
