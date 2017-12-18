{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

{-|
Module      : Storage -> Persistance
Description : Persistance component.
Copyright   : >implying
License     : >implying
Maintainer  : Florian Hageneder
Stability   : none
Portability : what?
-}
module Storage (getGame, loadGames) where

import Game (Game (..))
import Control.Exception
import System.IO
import Data.List (find)

gameFile :: FilePath
gameFile = "storage.dat"

getGames :: [Game]
getGames = do
  value <- (try loadGames :: IO (Either SomeException [Game])) :: (Either SomeException [Game])
  -- case value of
    -- Left ex -> []
    -- Right games -> games
  return value

getGame :: Int -> Maybe Game
getGame gid
  | gid < 0 = Nothing
  | otherwise =  Nothing

    -- let result = try loadGames
                    -- in case result of
                      -- Left ex -> Nothing
                      -- Right games -> Maybe find (\g -> gameId g == gid) games

-- Helper #####################################################################

saveGames :: [Game] -> IO ()
saveGames games = writeFile gameFile (show games)

loadGames :: IO [Game]
loadGames = do
  content <- readFile gameFile
  let games :: [Game]
      games = read content
      in return games
