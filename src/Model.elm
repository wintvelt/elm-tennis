module Model exposing (..)

type GamesUpToFive
    = Zero
    | One
    | Two
    | Three
    | Four
    | Five


type WinScore
    = MaxFive GamesUpToFive
    | Six

type alias SetScore =
    { winner : Player 
    , winScore : WinScore
    }

type alias GameScores =
    { player1 : GamesUpToFive
    , player2 : GamesUpToFive
    }


type SetInProgress
    = UpToFive GameScores
    | SixToFive Player
    | TieBreak

type SetResult =
    SameSet SetInProgress
    | SetWon SetScore


type Player
    = PlayerOne
    | PlayerTwo


type alias FirstSet =
    { setwinner : Player
    , looserscore : Games
    }


type alias SecondSet =
    { secondsetwinner : Player
    , alreadywonfirst : Bool
    , secondlooserscores : ( Games, Games )
    }


type alias ThirdSet =
    { wonthirdandsecond : Bool
    , thirdsetlooserscores : ( Games, Games, Games )
    }


type alias FourthSet =
    { foursthsetlooserscores : ( Games, Games, Games, Games )
    }


type alias OtherSets =
    Maybe SecondSet (Maybe ThirdSet (Maybe FourthSet))


type Previous
    = PlayedOne FirstSet
    | PlayedMore OtherSets


type alias Model =
    { previous : Maybe (Previous)
    , current : SetInProgress
    }