{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
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

import Game (Game (..), newGame)
import Player
import Data.Maybe
import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

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
        :<|> "games" :> Post '[JSON] Game -- Create session
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
          allGames = return [ ]

          createGame :: Handler Game
          createGame = return $ fromJust (newGame "Test")

          getGame :: Int -> Handler Game
          getGame gid = return $ fromJust (newGame "Test")

          turnGame :: Int -> TurnMessage -> Handler Game
          turnGame gid msg = return $ fromJust (newGame "Test")

          solveGame :: Int -> TurnMessage -> Handler Game
          solveGame gid msg = return $ fromJust (newGame "Test")

          createPlayer :: Int -> Handler Player
          createPlayer gid = return $ newPlayer 0

-- ############################################################################
-- Boilerplate

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

startApp :: IO ()
startApp = run 8080 app
