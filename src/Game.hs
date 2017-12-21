{-# LANGUAGE DataKinds       #-}
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
module Game (Game (..), newGame, makeATurn, makeASolve, nextPlayerAlive) where

import Data.Aeson
import Data.Aeson.TH

import Word
import Player
import Data.List
import Data.Maybe

-- | The type to store a game sessions state
data Game = Game
  { gameId :: Int -- ^ ID of the game
  , solution :: SolutionWord -- ^ SolutionWord the game is about
  , players :: [Player] -- ^ all participating player
  , isRunning :: Bool -- ^ wether the game is still running
  , atTurn :: Player  -- ^ the player to make the next turn
  , guesses :: String -- ^ all chars tried in the past
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

-- | Updates the given game session and transists it to the next state
makeASolve::
  Player -- ^ The player who wants to solve the game
  -> String -- ^ The solution to try
  -> Game -- ^ The game to operate on
  -> (Game, Bool) -- ^ The updated game
makeASolve p try g
  | not (validTurn p ' ' g) = (g, False)
  | fst $ solveWord try (solution g) = do -- Player actually solved game
      let solved = snd $ solveWord try (solution g)
      (g {solution = solved, isRunning = False}, True)
  | otherwise = do -- Player did not solve -> now he is dead
      let newPlayers = updatePlayers (killPlayer p) (players g)
      (g {players = newPlayers, atTurn = nextPlayerAlive g}, False)

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
validTurn p _ g
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
  in let tmpGame = g {players= newPlayers, guesses= newGuesses}
      in tmpGame {atTurn= nextPlayerAlive tmpGame}

playersSuccess ::
 Player
 -> Char
 -> Game
 -> Game
playersSuccess p c g =
  let newSolution = solveChar c (solution g)
    in g {solution= newSolution, isRunning= isPlayable newSolution, atTurn= nextPlayerAlive g, guesses= guesses g ++ [c] }
