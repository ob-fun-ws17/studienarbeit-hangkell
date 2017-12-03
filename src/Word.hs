{-|
Module      : Hangman Word Module
Description : Library to handle a word currently being played.
-}
module Word
    () where

import Data.Char(toLower)

-- | Checks if a given string is in the word.
tryChar ::
  Char -- ^ The char the player played
  -> String -- ^ The word that is being played3
  -> Bool -- ^ wether guessed char was right
tryChar try word = toLower try `elem` map toLower word
