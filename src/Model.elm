module Model exposing (..)

type Player
    = PlayerOne
    | PlayerTwo


type SetInProgress
    = UpToFive GameScores
    | SixToFive Player
    | TieBreak

type alias GameScores =
    { player1 : GamesUpToFive
    , player2 : GamesUpToFive
    }

type GamesUpToFive
    = Zero
    | One
    | Two
    | Three
    | Four
    | Five

type SetResult =
    SameSet SetInProgress
    | SetWon SetScore

type alias SetScore =
    { winner : Player 
    , winScore : WinScore
    }

type WinScore
    = MaxFive GamesUpToFive
    | Six


type alias FirstSet =
    SetScore

type alias SecondSet =
    { setWinner : Player
    , winScore : WinScore
    , firstSet : SetScore
    }


type alias ThirdSet =
    { leader : Player
    , leaderLost : OneSet
    , setScores : ( WinScore, WinScore, WinScore )
    }

type OneSet =
    Set1
    | Set2
    | Set3

type alias FourthSet =
    { setWinner : Player
    , wonEarlier : OneSet
    , setScores : ( WinScore, WinScore, WinScore, WinScore )
    }

type Previous
    = PlayedNone
    | PlayedOne FirstSet
    | PlayedTwo SecondSet
    | PlayedThree ThirdSet


type alias Model =
    { previous : Maybe (Previous)
    , current : SetInProgress
    }