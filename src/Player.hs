{-|
Module      : Hangman player Module
Description : Library to handle a single or group of players.
Copyright   : >implying
License     : >implying
Maintainer  : Florian Hageneder
Stability   : none
Portability : what?
-}
module Player where

maxFaliures = 10

type Id = Int
type Secret = String
type Failures = Int
type Alive = Bool
type Player = (Id, Secret, Failures, Alive)

{- | Creates a new Player with the given id.
The player is initialized with NO secret and alive

newPlayer 4
>>> (4, "", 0, True)
-}
newPlayer ::
  Int -- ^ the id for the player
  -> Player -- ^ created player
newPlayer pid = (pid, "", 0, True)

{- | returns a list of all players alive -}
playersAlive ::
  [Player] -- ^
  -> [Player]
playersAlive = filter (\p@(_,_,_, state) -> state)

{- | Updates the player after an wrong try. Kills him after last failure -}
wrongGuess ::
  Player -- ^ player that guessed wrong
  -> Player -- ^ updated player
wrongGuess p@(_, _, _, False) = p
wrongGuess (a, b, failures, alive) = let newfailures = failures + 1
                                        in (a, b, newfailures, newfailures < maxFaliures)
