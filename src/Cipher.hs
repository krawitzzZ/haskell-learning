module Cipher where

import           Data.Char

data Side = L | R deriving (Eq, Show, Ord)

minLower :: Int
minLower = ord 'a'

maxLower :: Int
maxLower = ord 'z'

minCapit :: Int
minCapit = ord 'A'

maxCapit :: Int
maxCapit = ord 'Z'

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


cipherMain :: IO ()
cipherMain = do
  let sentence = "Hello there! What's up?"
  let tmp      = caesar R 15 sentence
  putStrLn "Cipher:"
  putStrLn $ "sentence: " ++ sentence
  putStrLn $ "encoded: " ++ tmp
  putStrLn $ "decoded: " ++ uncaesar R 15 tmp
  putStrLn "\n"
