{-|
Module      : Hangman Game Module
Description : Library to handle game logic.
Copyright   : >implying
License     : >implying
Maintainer  : Florian Hageneder
Stability   : none
Portability : what?
-}
module Game where

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

makeATurn::
  Player -- ^ The player who wants to make a turn
  -> Char -- ^ The char to try
  -> Game -- ^ The game to operate on
  -> (Game, Bool) -- ^ The updated game
makeATurn _ _ g@(_,_,False,_,_) = (g, False) -- No turn on ended game
makeATurn p@(pId, pSec, pFailures, pAlive) char g@(solution, players, running, playerAtTurn, guessed)
  | p /= playerAtTurn = (g, False)  -- Only the player at turn
  | not pAlive = ((solution, players, running, nextPlayerAlive g, guessed), False) -- player at turn should not be dead
  | char `elem` guessed = (g, False) -- Only chars not tried until now -- failure!
  | otherwise = if tryChar char solution
      then  ((solveChar char solution, players, isPlayable $ solveChar char solution, nextPlayerAlive g, guessed ++ [char]), True) -- Char was solved in game
      else let newPlayers = updatePlayers (wrongGuess p) players
                in ((solution, newPlayers, running, nextPlayerAlive (solution, newPlayers, running, playerAtTurn, guessed), guessed ++ [char]), True) -- Char was solved but not in solution


{- | Returns the player which is currently at turn. -}
playerAtTurn ::
  Game -- ^ Game to check
  -> Maybe Player -- ^ Player that is at turn.
playerAtTurn (_,_,state,p,_)
          | state = Just p
          | otherwise = Nothing

{- returns the next alive player that is at turn.-}
-- Possibly crashes when no one is alive!!
nextPlayerAlive::
  Game
  -> Player
nextPlayerAlive (_, players, _, turn@(turnId,_,_,_), _) =
  let index = fromMaybe (-1) (findIndex (\(curID,_,_,_) -> curID == turnId) players)
      in head $ playersAlive ((\(a, b) -> b ++ a) (splitAt index players))

-- * Helper

trimPlayers::
  Game
  -> Game
trimPlayers (a, players, c, d, e) = (a, filter (\(_,_,_, alive) -> alive) players , c, d, e)

{- | Takes an updated player and replaces its old version in the list.-}
updatePlayers::
  Player -- ^ The updated player
  -> [Player] -- ^ Old players
  -> [Player] -- ^ updated players
updatePlayers x@(pid,_,_,_) = map (\p@(cid,_,_,_) -> if cid == pid then x else p)
