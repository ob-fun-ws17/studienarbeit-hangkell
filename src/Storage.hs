-- {-# LANGUAGE DataKinds       #-}
-- {-# LANGUAGE TypeOperators   #-}

{-|
Module      : Storage -> Persistance
Description : Persistance component.
Copyright   : >implying
License     : >implying
Maintainer  : Florian Hageneder
Stability   : none
Portability : what?
-}
-- module Storage (loadGames, loadGame, saveGames, saveGame) where
module Storage where

import Game (Game (..))
import Control.Exception
import System.IO
import Data.List (find)

import Control.Monad.IO.Class
import Control.Monad (when)

-- | The storage to keep all game sessions
gameFile :: FilePath
gameFile = "storage.dat"

-- ###########################################################################
-- Read games

-- | Returns all the stored games.
loadGames :: IO [Game] -- ^ Loads all stored games from the storage
loadGames = do
  content <- readFile gameFile
  return $ read content

-- | Returns a single stored game or Nothing
loadGame ::
  Int -- ^ GameID to get
  -> IO (Maybe Game)
loadGame gid = do
  games <- loadGames
  return $ find (\g -> gameId g == gid) games

-- ###########################################################################
-- Store games

-- | Overrides the whole storage with the given set of games
saveGames ::
  [Game] -- ^ All games that should be persisted in the storage
  -> IO () -- ^ writes everything the storage
saveGames games = writeFile gameFile (show games)

-- | Overrides the stored game with same ID or appends it to the list
saveGame ::
  Game -- ^ Single game to update
  -> IO () -- ^ Writes result to the file
saveGame update = do
  games <- loadGames
  let newGames = updateGames games update
  when (length newGames >= 0) $
    writeFile gameFile  (show $ updateGames games update)


updateGames ::
  [Game] -- ^ List of games that should be updated
  -> Game -- ^ Game that should be updated or added
  -> [Game] -- ^ updated list
updateGames input update =
  if not (any (\ g -> gameId g == gameId update) input)
    then input ++ [update]
    else map (\g -> if gameId g == gameId update then update else g) input
