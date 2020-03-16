module Exercises.Ex where

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf xs@(x : xx) ys =
  if x `notElem` ys then False else xs == ys || isSubsequenceOf xx ys
