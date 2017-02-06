module Game exposing (..)

{- Game

   type Game(..)
   type GameOn

   newGame : GameOn
   pointInGame : Player -> GameOn -> Game
   getGamePoints : GameOn -> (String, String)

      each player can score points, which are counted as follows
      Love - 15 - 30 - 40 - game

      "love" means no points
      "game" means a player has won

      When both players have scored three times, the game score is called
      "deuce" (equivalent of 40-40)

      To win from a deuce Game, one player needs a two-point difference,
      If a player scores once from deuce,
      the score for that player is called "advantage"
      That player can score another point to win the game,
      otherwise the Game goes back to deuce.

      only the winner of that game is saved
      the final score of an individual game is not saved,

-}

import Player exposing (Player(..))


{- Game score for an individual player -}


type GameScore
    = GameLove
    | Game15
    | Game30
    | Game40
    | GameA


type GameOn
    = GameOn GameScore GameScore



{- type exposed to other tennis game modules -}


type Game
    = SameGame GameOn
    | GameWon


newGame : GameOn
newGame =
    GameOn GameLove GameLove



-- helpers to award a point to a player


pointInGame : Player -> GameOn -> Game
pointInGame player gameOn =
    case ( player, gameOn ) of
        ( Player1, GameOn GameA _ ) ->
            GameWon

        ( Player1, GameOn Game40 Game40 ) ->
            GameOn GameA Game40
                |> SameGame

        ( Player1, GameOn Game40 GameA ) ->
            GameOn Game40 Game40
                |> SameGame

        ( Player1, GameOn Game40 pl2games ) ->
            GameWon

        ( Player1, GameOn Game30 pl2games ) ->
            GameOn Game40 pl2games
                |> SameGame

        ( Player1, GameOn Game15 pl2games ) ->
            GameOn Game30 pl2games
                |> SameGame

        ( Player1, GameOn GameLove pl2games ) ->
            GameOn Game15 pl2games
                |> SameGame

        ( Player2, GameOn _ GameA ) ->
            GameWon

        ( Player2, GameOn Game40 Game40 ) ->
            GameOn Game40 GameA
                |> SameGame

        ( Player2, GameOn GameA Game40 ) ->
            GameOn Game40 Game40
                |> SameGame

        ( Player2, GameOn pl1games Game40 ) ->
            GameWon

        ( Player2, GameOn pl1games Game30 ) ->
            GameOn pl2games Game40
                |> SameGame

        ( Player2, GameOn pl1games Game15 ) ->
            GameOn pl1games Game30
                |> SameGame

        ( Player2, GameOn pl1games GameLove ) ->
            GameOn pl1games Game15
                |> SameGame



-- helpers to get a string values for the score


getGamePoints : GameOn -> ( String, String )
getGamePoints (GameOn pl1games pl2games) =
    ( pointsToStr pl1games, pointsToStr pl2games )


pointsToStr : GameScore -> String
pointsToStr gameScore =
    case gameScore of
        GameLove ->
            "Love"

        Game15 ->
            "15"

        Game30 ->
            "30"

        Game40 ->
            "40"

        GameA ->
            "A"
