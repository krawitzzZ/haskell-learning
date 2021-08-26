module Randomness.Main where

import           System.Random
import           Control.Monad                  ( unless )

coins :: RandomGen g => g -> (Bool, Bool, Bool)
coins gen =
  let (first , newGen ) = random gen
      (second, newGen') = random newGen
      (third , _'     ) = random newGen'
  in  (first, second, third)

moreCoins :: RandomGen g => g -> [Bool]
moreCoins = take 10 . randoms

finiteRandoms :: RandomGen g => Integer -> g -> ([Int], g)
finiteRandoms n gen = finiteRandomsIn (minBound :: Int) (maxBound :: Int) n gen

finiteRandomsIn :: RandomGen g => Int -> Int -> Integer -> g -> ([Int], g)
finiteRandomsIn _ _ 0 gen = ([], gen)
finiteRandomsIn x y n gen = (r : rs, lastGen)
 where
  (r , newGen ) = randomR (x, y) gen
  (rs, lastGen) = finiteRandomsIn x y (n - 1) newGen

randomIn :: RandomGen g => Int -> Int -> g -> (Int, g)
randomIn x y gen = randomR (x, y) gen

someRands :: (RandomGen g) => Char -> Char -> Int -> g -> String
someRands x y n = take n . randomRs (x, y)

someIoRands :: Char -> Char -> IO ()
someIoRands x y = do
  gen <- getStdGen
  putStrLn $ someRands x y 10 gen


guessTheNumber :: IO ()
guessTheNumber = do
  gen <- getStdGen
  askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
  let (number, newGen) = randomR (1, 5) gen :: (Int, StdGen)

  putStrLn "Guess a number from 1 to 5:"
  inputNumStr <- getLine

  unless (null inputNumStr) $ do
    let num = read inputNumStr
    if num == number
      then print "you are right!"
      else print $ "sorry, it was " ++ show number
    askForNumber newGen
