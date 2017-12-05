{-|
Module      : Hangman Game Module
Description : Library to handle game logic.
Copyright   : >implying
License     : >implying
Maintainer  : Florian Hageneder
Stability   : none
Portability : what?
-}
module Game where

type Id = Int
type Secret = String
type Guesses = Int
type Alive = Bool
type Player = (Id, Secret, Guesses, Alive)
