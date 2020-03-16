module VigenereCipher
  ( vigenere
  , devigenere
  , Encryption(..)
  )
where

import           Data.Char
import           Data.List
import           Data.Maybe
import qualified CaesarCipher                  as CC

data Encryption = Encrypt | Decrypt deriving (Eq, Show, Ord)

capitalLetters :: String
capitalLetters = ['A' .. 'Z']

lowLetters :: String
lowLetters = ['a' .. 'z']

numbers :: [Integer]
numbers = [0 .. fromIntegral $ length lowLetters - 1]

lowLettersZip :: [(Integer, Char)]
lowLettersZip = zip numbers lowLetters

capitalLettersZip :: [(Integer, Char)]
capitalLettersZip = zip numbers capitalLetters

zipFallback :: (Integer, Char)
zipFallback = (0, ' ')

vigenere :: String -> String -> String
vigenere sentence keyWord =
  encode Encrypt sentence (withKeyword sentence keyWord)

devigenere :: String -> String -> String
devigenere sentence keyWord =
  encode Decrypt sentence (withKeyword sentence keyWord)

withKeyword :: String -> String -> String
withKeyword sentence keyWord = take (length sentence) (cycle keyWord)

encode :: Encryption -> String -> String -> String
encode encr sentence keyWord = concatMap encode' $ zip (cycle keyWord) sentence
 where
  encode' (c1, c2) = CC.caesar encr' (getShift c1) [c2]
  getShift c' | isUpper c' = fromZip c' capitalLettersZip
              | otherwise  = fromZip c' lowLettersZip
  fromZip char list = fromIntegral $ fst $ fromMaybe
    zipFallback
    (find (\(_, c) -> c == char) list)
  encr' = if encr == Encrypt then CC.R else CC.L
