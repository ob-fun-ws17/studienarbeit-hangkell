{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module      : Library
Description : Library to handle __RESTful__ API
Copyright   : >implying
License     : >implying
Maintainer  : Florian Hageneder
Stability   : none
Portability : what?
-}
module Api
    ( startApp
    , app
    ) where

import Game (Game (..), newGame, makeATurn)
import Player
import Storage
import Data.Maybe
import Data.Aeson
import Data.Aeson.TH
import Data.List
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Control.Monad.IO.Class
import Control.Monad (when)

-- ############################################################################
-- Messages

data TurnMessage = TurnMessage
  { playerId :: Int
  , playerSecret :: String
  , guess :: String
  }
$(deriveJSON defaultOptions ''TurnMessage)

-- ############################################################################
-- API

type API = "games" :> Get '[JSON] [Game] -- Get all games
        :<|> "games" :> QueryParam "word" String :> Post '[JSON] Game -- Create session
        :<|> "games" :> Capture "gid" Int :> Get '[JSON] Game -- Get single game
        :<|> "games" :> Capture "gid" Int :> ReqBody '[JSON] TurnMessage :> Put '[JSON] Game -- make a Turn
        :<|> "games" :> Capture "gid" Int :> ReqBody '[JSON] TurnMessage :> Put '[JSON] Game -- solve Game
        :<|> "games" :> Capture "gid" Int :> "players" :> Post '[JSON] Player -- create Player


server :: Server API
server = allGames
    :<|> createGame
    :<|> getGame
    :<|> turnGame
    :<|> solveGame
    :<|> createPlayer

    where allGames :: Handler [Game]
          allGames = liftIO loadGames

          createGame :: Maybe String -> Handler Game
          createGame Nothing = throwError err400 { errBody = "Parameter word missing" }
          createGame word = do
            games <- liftIO loadGames
            let ids = map gameId games
            let game = fromJust (newGame (if null ids then 0 else maximum ids + 1) $ fromJust word)
            liftIO $ saveGame game
            return game

          getGame :: Int -> Handler Game
          getGame = getGameHandled

          turnGame :: Int -> TurnMessage -> Handler Game
          turnGame gid change = do
            game <- liftIO $ loadGame gid
            case game of
              Nothing -> throwError err404 { errBody = "There is no game with this ID" }
              Just g -> do
                when (length (Api.guess change) /= 1) $ throwError err400 { errBody = "You can only guess a single char!" }
                player <- getPlayerHandled g (Api.playerId change) (Api.playerSecret change)
                let game = fst (makeATurn player (head $ Api.guess change) g)
                liftIO $ saveGame game
                return game


          solveGame :: Int -> TurnMessage -> Handler Game
          solveGame gid msg = return $ fromJust (newGame 0 "Test")

          createPlayer :: Int -> Handler Player
          createPlayer gid = return $ newPlayer 0

-- ############################################################################
-- Boilerplate

-- | Loads a game or throws 404 if it could not be found
getGameHandled :: Int -> Handler Game
getGameHandled gid = do
  game <- liftIO $ loadGame gid
  case game of
    Nothing -> throwError err404 { errBody = "There is no game with this ID" }
    Just g -> return g

-- | Fetches the player from the game or throws an error
getPlayerHandled :: Game -> Int -> String -> Handler Player
getPlayerHandled game pid key = do
  let player = getPlayerForId (players game) pid key
  case player of
    Nothing -> throwError err401 { errBody = "There is no player with this ID or credentials did not match" }
    Just p -> return p

-- ############################################################################
-- Boilerplate

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

startApp :: IO ()
startApp = run 8080 app
