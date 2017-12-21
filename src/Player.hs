{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Data.List (find)

maxFaliures = 10

-- | Data type to bundle Imformation about a player within an game session.
data Player = Player
  { playerId :: Int -- ^ ID of the player
  , secret :: String -- ^ The secret key of the player
  , failures :: Int -- ^ The amount of failures the player had
  , isAlive :: Bool -- ^ If the player is still alive
  } deriving (Eq, Show, Read)

instance ToJSON Player where
  toJSON (Player pid secret failures isAlive) = object
    [ "playerID" .= pid
    , "failures" .= failures
    , "alive" .= isAlive
    , "maxFailures" .= Player.maxFaliures
    ]

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

-- | Grants access to a player with given id if the key does match
getPlayerForId ::
  [Player] -- ^ The set of players to search in
  -> Int -- ^ The id of the player to fetch
  -> String -- ^ The secret key of the player
  -> Maybe Player -- ^ A player or Nothing if id or key did not match
getPlayerForId [] _ _ = Nothing
getPlayerForId _ _ "" = Nothing
getPlayerForId xs pid key
  | pid < 0 = Nothing
  | otherwise = do
      let player = find (\x -> playerId x == pid) xs
      case player of
        Nothing -> Nothing
        Just p -> if secret p == key
                    then return p
                    else Nothing
