{-|
Module      : Hangman Game Module
Description : Library to handle game logic.
Copyright   : >implying
License     : >implying
Maintainer  : Florian Hageneder
Stability   : none
Portability : what?
-}
module Game (Game, makeATurn, playerAtTurn, nextPlayerAlive) where
--module Game where

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
makeATurn p@(pId, pSec, pFailures, pAlive) char g@(solution, players, running, playerAtTurn, guessed)
  | not (validTurn p char g) = (g, False)
  | char `elem` guessed = (playersMistake p char g, True)
  | tryChar char solution = (playersSuccess p char g, True)
  | otherwise = (playersMistake p char g, True)

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
      in head $ playersAlive ((\(a, b) -> b ++ a) (splitAt (index + 1) players))

-- * Helper

validTurn ::
  Player
  -> Char
  -> Game
  -> Bool
validTurn p@(a, b, c, alive) char g@(solution, players, running, playerAtTurn, guessed)
  | not running = False -- No turn on ended game
  | not alive = False -- player at turn should not be dead
  | p /= playerAtTurn = False -- Only the player at turn
  | otherwise = True

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

playersMistake ::
  Player
  -> Char
  -> Game
  -> Game
playersMistake p c g@(solution, players, running, atTurn, guesses) =
  let newPlayers = updatePlayers (wrongGuess p) players;
      newGuesses = if c `elem` guesses then guesses else guesses ++ [c];
  in let tmpGame = (solution, newPlayers, running, atTurn, newGuesses);
     in (solution, newPlayers, running, nextPlayerAlive tmpGame, newGuesses)

playersSuccess ::
 Player
 -> Char
 -> Game
 -> Game
playersSuccess p c g@(solution, players, running, atTurn, guesses) =
  let tmpGame = (solution, players, running, atTurn, guesses);
  in (solution, players, running, nextPlayerAlive tmpGame, guesses ++ [c])
