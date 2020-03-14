module CaesarCipher where

import           Data.Char

data Side = L | R deriving (Eq, Show, Ord)

caesar :: Side -> Int -> String -> String
caesar side shift = map $ encode side shift

uncaesar :: Side -> Int -> String -> String
uncaesar side = caesar anotherSide
  where anotherSide = if side == L then R else L

encode :: Side -> Int -> Char -> Char
encode side shift char
  | char == ' '  = '}'
  | char == '}'  = ' '
  | char == ','  = '{'
  | char == '{'  = ','
  | char == '.'  = '.'
  | char == '!'  = '/'
  | char == '/'  = '!'
  | char == '?'  = '^'
  | char == '^'  = '?'
  | char == '\'' = '~'
  | char == '~'  = '\''
  | isUpper char = chr $ getCharPosition side minCapit maxCapit
  | otherwise    = chr $ getCharPosition side minLower maxLower
 where
  getCharPosition R low high =
    if newCharPos <= high then newCharPos else low + (newCharPos - high) - 1
  getCharPosition L low high =
    if newCharPos >= low then newCharPos else high - (low - newCharPos) + 1
  newCharPos =
    if side == R then ord char + getShift shift else ord char - getShift shift
  getShift shift' = if shift' `div` lettersNum > 0
    then getShift (shift' `mod` lettersNum)
    else shift'
  minLower   = ord 'a'
  maxLower   = ord 'z'
  minCapit   = ord 'A'
  maxCapit   = ord 'Z'
  lettersNum = maxLower - minLower + 1
