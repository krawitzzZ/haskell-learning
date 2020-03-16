module Exercises.Ex where

import           Data.Char

import           Foldable.Lists                 ( splitOn )

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf xs@(x : xx) ys =
  if x `notElem` ys then False else xs == ys || isSubsequenceOf xx ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords [] = [("", "")]
capitalizeWords word =
  map (\word'@(x : xs) -> (word', toUpper x : xs)) $ splitOn ' ' word
