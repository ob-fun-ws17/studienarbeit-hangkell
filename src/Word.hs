{-|
Module      : Hangman Word Module
Description : Library to handle a hangman solution word.
-}
module Word
    (tryChar, charPositions) where

import Data.Char(toLower)
import Data.List

-- | Checks if a given string is in the word.
tryChar ::
  Char -- ^ The char the player played
  -> String -- ^ The word that is being played3
  -> Bool -- ^ wether guessed char was right
tryChar try solution = null $ charPositions try solution

-- | Returns positions of a char in the given word.
charPositions ::
  Char -- ^ Char to search for
  -> String -- ^ Solution word to search in
  -> [Int] -- ^ List of positions in solution.
charPositions try solution = elemIndices (toLower try) (map toLower solution)
