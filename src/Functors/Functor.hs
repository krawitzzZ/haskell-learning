module Functors.Functor where

import           Data.Char
import           Data.List


-- functor laws
-- 1) fmap id = id
-- 2) fmap (f . g) = fmap f . fmap g


main :: IO ()
main = do
  putStrLn "Type something"
  line <- fmap reverse getLine
  putStrLn line
  putStrLn ""
  line' <- fmap (intersperse '*' . reverse . (++ "!") . map toUpper) getLine
  putStrLn line
  putStrLn ""
  putStrLn line'
  putStrLn ""
  print $ functionFunctor 3
  putStrLn ""

functionFunctor :: Integer -> Integer
functionFunctor = fmap (+ 5) (* 2)

data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where
  fmap _ CNothing          = CNothing
-- we change the counter so the first law is broken: id does not return the same result
  fmap f (CJust counter y) = CJust (counter + 1) (f y)

data Probably a = Nope | Exactly a deriving (Show)

instance Functor Probably where
  fmap _ Nope        = Nope
  fmap f (Exactly a) = Exactly (f a)
