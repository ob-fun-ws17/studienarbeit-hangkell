module Main where

import Api
import Storage (gameFile)

import Control.Monad (unless)
import System.Directory (doesFileExist)
import System.IO (writeFile)

main :: IO ()
main = do
  -- Initialize storage if it is not present
  fileExist <- doesFileExist Storage.gameFile
  unless fileExist $ writeFile Storage.gameFile "[]"
  -- Start server
  startApp
