module Exercises.Ex where

import           Data.Char

import           Foldable.Lists                 ( splitOn )

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf xs@(x : xx) ys =
  if x `notElem` ys then False else xs == ys || isSubsequenceOf xx ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords []   = [("", "")]
capitalizeWords word = map capitalize $ splitOn ' ' word
 where
  capitalize ""             = ("", "")
  capitalize word'@(x : xs) = (word', toUpper x : xs)

capitalizeWord :: String -> String
capitalizeWord ""         = ""
capitalizeWord (' ' : xs) = ' ' : capitalizeWord xs
capitalizeWord (x   : xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph "" = ""
capitalizeParagraph p  = foldl combine "" $ map capitalizeWord sentences
 where
  sentences = splitOn '.' p
  combine res s = res ++ s ++ "."
