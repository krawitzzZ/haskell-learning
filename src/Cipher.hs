module Cipher
  ( caesar
  , unCaesar
  )
where

import           Data.Char

data Side = L | R deriving (Eq, Show, Ord)

minLower = ord 'a'
maxLower = ord 'z'
minCapit = ord 'A'
maxCapit = ord 'Z'

caesar :: Side -> Int -> String -> String
caesar side shift = map $ encode side shift

unCaesar :: Side -> Int -> String -> String
unCaesar side = caesar anotherSide
  where anotherSide = if side == L then R else L

encode :: Side -> Int -> Char -> Char
encode side shift char
  | char == ' '  = '}'
  | char == '}'  = ' '
  | char == ','  = '{'
  | char == '{'  = ','
  | char == '.'  = '.'
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
  newCharPos = if side == R then ord char + shift else ord char - shift
