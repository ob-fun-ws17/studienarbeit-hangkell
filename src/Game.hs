{-|
Module      : Hangman Game Module
Description : Library to handle game logic.
Copyright   : >implying
License     : >implying
Maintainer  : Florian Hageneder
Stability   : none
Portability : what?
-}
module Game
  (Game, playerAtTurn, nextPlayerAlive) where

import Word
import Player
import Data.List
import Data.Maybe

type Game = (SolutionWord {- The word this game is about to solve -}
            , [Player] {- All players participating at the game -}
            , Bool {- Is game still active/running -}
            , Player {- Player currently at turn -}
            , String {- All guessed chars -}
            )

{- | Returns the player which is currently at turn. -}
playerAtTurn ::
  Game -- ^ Game to check
  -> Maybe Player -- ^ Player that is at turn.
playerAtTurn (_,_,state,p,_)
          | state = Just p
          | otherwise = Nothing

{- returns the next alive player that is at turn.-}
-- Possibly crashes when no one is alive!!
nextPlayerAlive ::
  Game
  -> Player
nextPlayerAlive (_, players, _, turn@(turnId,_,_,_), _) =
  let index = fromMaybe (-1) (findIndex (\(curID,_,_,_) -> curID == turnId) players)
      in head $ playersAlive ((\(a, b) -> b ++ a) (splitAt index players))

-- * Helper

trimPlayers ::
  Game
  -> Game
trimPlayers (a, players, c, d, e) = (a, filter (\(_,_,_, alive) -> alive) players , c, d, e)
