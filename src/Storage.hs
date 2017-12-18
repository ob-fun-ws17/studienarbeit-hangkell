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
module Storage (getGame, saveGames, loadGames) where

import Game
import Player
import System.IO
import Data.Aeson
import Data.Aeson.TH

p1 = Player 0 "" (Player.maxFaliures - 1) True
p2 = Player 1 "" 0 True
testGame :: Game
testGame = Game 0 [('a', False)]
            [p1, p2]
            True
            p1
            "b"

gameFile :: FilePath
gameFile = "storage.dat"

getGame :: Int -> Maybe Game
getGame gid
  | gid < 0 = Nothing
  | otherwise = Nothing

saveGames :: IO ()
saveGames = writeFile gameFile (show [testGame])

loadGames :: IO [Game]
loadGames = do
  content <- readFile gameFile
  let games :: [Game]
      games = read content
      in return games
