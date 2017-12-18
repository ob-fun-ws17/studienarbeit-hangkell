{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-|
Module      : Hangman player Module
Description : Library to handle a single or group of __players.__
Copyright   : >implying
License     : >implying
Maintainer  : Florian Hageneder
Stability   : none
Portability : what?
-}
module Player where

import Data.Aeson
import Data.Aeson.TH

maxFaliures = 10

-- | Data type to bundle Imformation about a player within an game session.
data Player = Player {
  playerId :: Int,
  secret :: String,
  failures :: Int,
  isAlive :: Bool
} deriving (Eq, Show, Read)

$(deriveJSON defaultOptions ''Player)

{- | Creates a new Player with the given id.
The player is initialized with NO secret and alive

newPlayer 4
>>> (4, "", 0, True)
-}
newPlayer ::
  Int -- ^ the id for the player
  -> Player -- ^ created player
newPlayer pid = Player pid "" 0 True

{- | returns a list of all players alive -}
playersAlive ::
  [Player] -- ^ A set of players
  -> [Player] -- ^ All players currently allowed to play
playersAlive = filter isAlive

{- | Updates the player after an wrong try. Kills him after last failure -}
wrongGuess ::
  Player -- ^ player that guessed wrong
  -> Player -- ^ updated player
wrongGuess p
  | not $ isAlive p = p
  | otherwise = let newFailures = failures p + 1
                in Player (playerId p) (secret p) newFailures (newFailures < maxFaliures)
