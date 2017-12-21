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
  let xs = ["[","]"]
  unless fileExist $ writeFile Storage.gameFile $ unwords xs
  -- Start server
  startApp
