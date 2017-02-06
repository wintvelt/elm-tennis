module Match exposing (..)

{- Match

   type Match(..)
   type MatchOn
   type MatchOver

   pointInMatch : Player -> MatchOn -> Match
   getCurrentGamePoints : MatchOn -> (String, String)
   getCurrentSets : MatchOn -> List (String, String)
   getWinner : MatchOver -> Player
   getFinalSets : MatchOver -> LIst (String, String)

   a match holds of a maximum of 5 sets
   the first player who wins 3 sets wins the match.

   a fifth set will be played if both players win 2 sets
   the last (fifth) set is scored differently from a normal set.
   a normal set can be won with a score of 7 games against 6
   the last set continues until one player has a lead of 2 games

-}

import Game exposing (..)
import Set exposing (..)
import Player exposing (Player(..), otherPlayer)


type Match
    = SameMatch MatchOn
    | MatchWonBy MatchOver


type MatchOn
    = PlayingNormalSets NormalSets
    | PlayingLastSet TieAfter4Sets


newMatch : MatchOn
newMatch =
    { currentGame = newGame
    , currentSet = newSet
    , playedSets = NoSets
    }
        |> PlayingNormalSets


type alias NormalSets =
    { currentGame : GameOn
    , currentSet : SetOn
    , playedSets : PlayedSets
    }


type LastSetResult
    = SameLastSet TieAfter4Sets
    | LastSetWon WinScore


type PlayedSets
    = NoSets
    | LeadAfter1Set
        { leader : Player
        , firstSet : SetScore
        }
    | LeadAfter2Sets
        { leader : Player
        , firstSet : SetScore
        , secondSet : SetScore
        }
    | TieAfter2Sets
        { lastWon : Player
        , firstSet : SetScore
        , secondSet : SetScore
        }
    | LeadAfter3Sets
        { leader : Player
        , setLost : OneToThree
        , firstSet : SetScore
        , secondSet : SetScore
        , thirdSet : SetScore
        }


type PlayedSetsResult
    = SamePlayed PlayedSets
    | ToFive TieAfter4Sets
    | SetsWon WinScore


type alias TieAfter4Sets =
    { currentGame : GameOn
    , currentSet : LastSetOn
    , lastSetWonBy : Player
    , otherSetWon : OneToThree
    , firstSet : SetScore
    , secondSet : SetScore
    , thirdSet : SetScore
    , fourthSet : SetScore
    }


type OneToThree
    = IsOne
    | IsTwo
    | IsThree


type alias MatchOver =
    { winner : Player
    , winScore : WinScore
    }


type WinScore
    = ThreeSetter
        { firstSet : SetScore
        , secondSet : SetScore
        , thirdSet : SetScore
        }
    | FourSetter
        { firstSet : SetScore
        , secondSet : SetScore
        , thirdSet : SetScore
        , fourthSet : SetScore
        , setLost : OneToThree
        }
    | FiveSetter
        { firstSet : SetScore
        , secondSet : SetScore
        , thirdSet : SetScore
        , fourthSet : SetScore
        , fifthSet : LastSetScore
        , leadAfter4Sets : Player
        , leadAlsoWon : OneToThree
        }


pointInMatch : Player -> MatchOn -> Match
pointInMatch player matchOn =
    case matchOn of
        PlayingNormalSets normalSets ->
            case pointInSet player normalSets of
                SameSetWithGame newNormalSets ->
                    newNormalSets
                        |> PlayingNormalSets

                SetWon setScore ->
                    case setInNormalSets player setScore normalSets of
                        SamePlayed newNormalSets ->
                            newNormalSets
                                |> PlayingNormalSets

                        ToFive fourSetter ->
                            fourSetter
                                |> PlayingLastSet

                        SetsWon winScore ->
                            { winner = player
                            , winScore = winScore
                            }
                                |> MatchWonBy

        PlayingLastSet tieAfter4Sets ->
            case pointInLastSet player tieAfter4Sets of
                SameLastSet sameTie ->
                    sameTie
                        |> PlayingLastSet

                LastSetWon winScore ->
                    winScore
                        |> MatchWonBy


setInNormalSets : Player -> SetScore -> NormalSets -> PlayedSetsResult
setInNormalSets player setScore { playedSets } =
    case playedSets of
        NoSets ->
            LeadAfter1Set
                { leader = player
                , firstSet = setScore
                }
                |> SamePlayed

        LeadAfter1Set previous ->
            if player == previous.leader then
                LeadAfter2Sets
                    { leader = player
                    , firstSet = previous.firstSet
                    , secondSet = setScore
                    }
                    |> SamePlayed
            else
                TieAfter2Sets
                    { lastWon = player
                    , firstSet = previous.firstSet
                    , secondSet = setScore
                    }
                    |> SamePlayed

        LeadAfter2Sets previous ->
            if player == previous.leader then
                ThreeSetter
                    { firstSet = previous.firstSet
                    , secondSet = previous.secondSet
                    , thirdSet = setScore
                    }
                    |> SetsWon
            else
                LeadAfter3Sets
                    { leader = otherPlayer player
                    , setLost = IsThree
                    , firstSet = previous.firstSet
                    , secondSet = previous.secondSet
                    , thirdSet = setScore
                    }
                    |> SamePlayed

        TieAfter2Sets previous ->
            LeadAfter3Sets
                { leader = player
                , setLost =
                    if previous.lastWon == player then
                        IsTwo
                    else
                        IsOne
                , firstSet = previous.firstSet
                , secondSet = previous.secondSet
                , thirdSet = setScore
                }
                |> SamePlayed

        LeadAfter3Sets previous ->
            if player == previous.leader then
                FourSetter
                    { firstSet = previous.firstSet
                    , secondSet = previous.secondSet
                    , thirdSet = previous.thirdSet
                    , fourthSet = setScore
                    , setLost = previous.setLost
                    }
                    |> SetsWon
            else
                { currentGame = newGame
                , currentSet = newLastSet
                , lastSetWonBy = player
                , otherSetWon = previous.setLost
                , firstSet = previous.firstSet
                , secondSet = previous.secondSet
                , thirdSet = previous.thirdSet
                , fourthSet = setScore
                }
                    |> ToFive


pointInLastSet : Player -> TieAfter4Sets -> LastSetResult
pointInLastSet player sets =
    case pointInGame player sets.currentGame of
        SameGame gameOn ->
            { sets | currentGame = gameOn }
                |> SameLastSet

        GameWon ->
            case gameInLastSet player sets.currentSet of
                SameLastSet lastSetOn ->
                    { sets
                        | currentGame = newGame
                        , currentSet = lastSetOn
                    }
                        |> SameLastSet

                LastSetWon ->
                    FiveSetter
                        { firstSet = sets.firstSet
                        , secondSet = sets.secondSet
                        , thirdSet = sets.thirdSet
                        , fourthSet = sets.fourthSet
                        , fifthSet = sets.currentSet
                        , leadAfter4Sets = sets.lastSetWonBy
                        , leadAlsoWon = sets.otherSetWon
                        }
                        |> SetsWon



--- Getters


getCurrentGamePoints : MatchOn -> ( String, String )
getCurrentGamePoints matchOn =
    case matchOn of
        PlayingNormalSets { currentGame } ->
            getGamePoints currentGame

        PlayingLastSet { currentGame } ->
            getGamePoints currentGame


getCurrentSets : MatchOn -> ( String, String )
getCurrentSets matchOn =
    case matchOn of
        PlayingNormalSets normalMatch ->
            getSetOnScores normalMatch.currentSet
                :: getSetScoresFromPrev normalMatch.playedSets

        PlayingLastSet set5 ->
            [ getLastSetOnScores set5.currentSet
            , getSetScores set5.leader set5.fourthSet
            ]
                ++ case set5.otherSetWon of
                    IsOne ->
                        [ getSetScores (otherPlayer set5.leader) thirdSet
                        , getSetScores (otherPlayer set5.leader) secondSet
                        , getSetScores set5.leader firstSet
                        ]

                    IsTwo ->
                        [ getSetScores (otherPlayer set5.leader) thirdSet
                        , getSetScores set5.leader secondSet
                        , getSetScores (otherPlayer set5.leader) firstSet
                        ]

                    IsThree ->
                        [ getSetScores set5.leader thirdSet
                        , getSetScores (otherPlayer set5.leader) secondSet
                        , getSetScores (otherPlayer set5.leader) firstSet
                        ]


getSetScoresFromPrev : PlayedSets -> List ( String, String )
getSetScoresFromPrev playedSets =
    case playedSets of
        NoSets ->
            []

        LeadAfter1Set { leader, firstSet } ->
            [ getSetScores leader firstSet ]

        LeadAfter2SetsSet { leader, firstSet, secondSet } ->
            [ getSetScores leader secondSet
            , getSetScores leader firstSet
            ]

        TieAfter2Sets { lastWon, firstSet, secondSet } ->
            [ getSetScores lastWon secondSet
            , getSetScores (otherPlayer lastWon) firstSet
            ]

        LeadAfter3Sets { leader, setLost, firstSet, secondSet, thirdSet } ->
            case setLost of
                IsOne ->
                    [ getSetScores leader thirdSet
                    , getSetScores leader secondSet
                    , getSetScores (otherPlayer leader) firstSet
                    ]

                IsTwo ->
                    [ getSetScores leader thirdSet
                    , getSetScores (otherPlayer leader) secondSet
                    , getSetScores leader firstSet
                    ]

                IsThree ->
                    [ getSetScores (otherPlayer leader) thirdSet
                    , getSetScores leader secondSet
                    , getSetScores leader firstSet
                    ]


getWinner : MatchOver -> Player
getWinner { winner } =
    winner


getFinalSets : MatchOver -> List ( String, String )
getFinalSets { winner, winScore } =
    case winScore of
        ThreeSetter { firstSet, secondSet, thirdSet } ->
            [ getSetScores winner thirdSet
            , getSetScores winner secondSet
            , getSetScores winner firstSet
            ]

        FourSetter { firstSet, secondSet, thirdSet, fourthSet, setLost } ->
            case setLost of
                IsOne ->
                    [ getSetScores winner fourthSet
                    , getSetScores winner thirdSet
                    , getSetScores winner secondSet
                    , getSetScores (otherPlayer winner) firstSet
                    ]

                IsTwo ->
                    [ getSetScores winner fourthSet
                    , getSetScores winner thirdSet
                    , getSetScores (otherPlayer winner) secondSet
                    , getSetScores winner firstSet
                    ]

                IsThree ->
                    [ getSetScores winner fourthSet
                    , getSetScores (otherPlayer winner) thirdSet
                    , getSetScores winner secondSet
                    , getSetScores winner firstSet
                    ]

        FiveSetter sets ->
            [ getLastSetScores winner.fifthSet
            , getSetScores sets.leadAfter4Sets sets.fourthSet
            ]
                ++ case sets.leadAlsoWon of
                    IsOne ->
                        [ getSetScores (otherPlayer sets.leadAlsoWon) thirdSet
                        , getSetScores (otherPlayer sets.leadAlsoWon) secondSet
                        , getSetScores sets.leadAlsoWon firstSet
                        ]

                    IsTwo ->
                        [ getSetScores (otherPlayer sets.leadAlsoWon) thirdSet
                        , getSetScores sets.leadAlsoWon secondSet
                        , getSetScores (otherPlayer sets.leadAlsoWon) firstSet
                        ]

                    IsThree ->
                        [ getSetScores sets.leadAlsoWon thirdSet
                        , getSetScores (otherPlayer sets.leadAlsoWon) secondSet
                        , getSetScores (otherPlayer sets.leadAlsoWon) firstSet
                        ]
