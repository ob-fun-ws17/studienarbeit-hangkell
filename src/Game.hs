{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hangman Game Module
Description : Library for handling a __game session__.
Copyright   : >implying
License     : >implying
Maintainer  : Florian Hageneder
Stability   : none
Portability : what?
-}
module Game (Game (..), newGame, makeATurn, playerAtTurn, nextPlayerAlive) where

import Data.Aeson
import Data.Aeson.TH

import Word
import Player
import Data.List
import Data.Maybe

-- | The type to store a game sessions state
data Game = Game
  { gameId :: Int -- ^ ID of the game
  , solution :: SolutionWord
  , players :: [Player]
  , isRunning :: Bool
  , atTurn :: Player
  , guesses :: String
  } deriving (Eq, Show, Read)

instance ToJSON Game where
  toJSON (Game gid solution players running atTurn guesses) = object
    [ "gameID" .= gid
    , "solution" .= showSolution solution
    , "players" .= toJSON players
    , "atTurn" .= toJSON atTurn
    , "guesses" .= guesses
    ]

-- | Creates a new game session with an completely unsolved given solutionWord
newGame::
  Int -- ^ The ID of this session
  -> String -- ^ The goal of this instance of Hangman
  -> Maybe Game -- ^ A valid new game or Nothing
newGame gid word
  | null word = Nothing
  | otherwise =
    let solution = createSolutionWord word
        player = newPlayer 0
        in Just $ Game gid solution [player] (isPlayable solution) player ""

-- | Updates the given game session and transists it to the next state
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

{- | returns the next alive player that is at turn.-}
-- Possibly crashes when no one is alive!!
nextPlayerAlive::
  Game
  -> Player
nextPlayerAlive g =
  let index = fromMaybe (-1) (findIndex (\p -> playerId p == playerId (atTurn g)) (players g) )
      in head $ playersAlive ((\(a, b) -> b ++ a) (splitAt (index + 1) (players g )))

-- * Helper ###################################################################

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
