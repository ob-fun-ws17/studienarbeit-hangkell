{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

{-|
Module      : Hangman Game Module
Description : Library to handle game logic.
Copyright   : >implying
License     : >implying
Maintainer  : Florian Hageneder
Stability   : none
Portability : what?
-}
module Game (Game (..), newGame, makeATurn, playerAtTurn, nextPlayerAlive) where
--module Game where

import Data.Aeson
import Data.Aeson.TH

import Word
import Player
import Data.List
import Data.Maybe

--type Game = (SolutionWord {- The word this game is about to solve -}
--            , [Player] {- All players participating at the game -}
--            , Bool {- Is game still active/running -}
--            , Player {- Player currently at turn -}
--            , String {- All guessed chars -}
--            )

data Game = Game {
  gameId :: Int,
  solution :: SolutionWord,
  players :: [Player],
  isRunning :: Bool,
  atTurn :: Player,
  guesses :: String
} deriving (Eq, Show, Read)

$(deriveJSON defaultOptions ''Game)

newGame::
  String
  -> Maybe Game
newGame word
  | null word = Nothing
  | otherwise =
    let solution = createSolutionWord word
        player = newPlayer 0
        in Just $ Game 0 solution [player] (isPlayable solution) player ""

makeATurn::
  Player -- ^ The player who wants to make a turn
  -> Char -- ^ The char to try
  -> Game -- ^ The game to operate on
  -> (Game, Bool) -- ^ The updated game
makeATurn p char g
  | not (validTurn p char g) = (g, False)
  | char `elem` guesses g = (playersMistake p char g, True)
  | tryChar char $ solution g = (playersSuccess p char g, True)
  | otherwise = (playersMistake p char g, True)

{- | Returns the player which is currently at turn. -}
playerAtTurn ::
  Game -- ^ Game to check
  -> Maybe Player -- ^ Player that is at turn.
playerAtTurn game
          | isRunning game = Just $ atTurn game
          | otherwise = Nothing

{- returns the next alive player that is at turn.-}
-- Possibly crashes when no one is alive!!
nextPlayerAlive::
  Game
  -> Player
nextPlayerAlive g =
  let index = fromMaybe (-1) (findIndex (\p -> playerId p == playerId (atTurn g)) (players g) )
      in head $ playersAlive ((\(a, b) -> b ++ a) (splitAt (index + 1) (players g )))

-- * Helper

validTurn ::
  Player
  -> Char
  -> Game
  -> Bool
validTurn p char g
  | not $ isRunning g = False -- No turn on ended game
  | not $ isAlive p = False -- player at turn should not be dead
  | p /= atTurn g = False -- Only the player at turn
  | otherwise = True

trimPlayers::
  Game
  -> Game
--trimPlayers game = game (a, filter (\(_,_,_, alive) -> alive) players game , c, d, e)
trimPlayers g = Game (gameId g) (solution g) (filter isAlive (players g)) (isRunning g) (atTurn g) (guesses g)

{- | Takes an updated player and replaces its old version in the list.-}
updatePlayers::
  Player -- ^ The updated player
  -> [Player] -- ^ Old players
  -> [Player] -- ^ updated players
updatePlayers x = map (\c-> if playerId c == playerId x then x else c)

playersMistake ::
  Player
  -> Char
  -> Game
  -> Game
playersMistake p c g =
  let newPlayers = updatePlayers (wrongGuess p) (players g);
      newGuesses = if c `elem` guesses g then guesses g else guesses g ++ [c];
  in let tmpGame = Game (gameId g) (solution g) newPlayers (isRunning g) (atTurn g) newGuesses
     in Game (gameId g) (solution g) newPlayers (isRunning g) (nextPlayerAlive tmpGame) newGuesses

playersSuccess ::
 Player
 -> Char
 -> Game
 -> Game
playersSuccess p c g =
  let newSolution = solveChar c (solution g)
  in Game (gameId g) newSolution (players g) (isPlayable newSolution) (nextPlayerAlive g) (guesses g ++ [c])
