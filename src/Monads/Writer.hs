module Monads.Writer where

import           Data.Monoid
import           Control.Monad.Writer


isBigGang :: Int -> (Bool, [String])
isBigGang x = (x > 9, ["Compared gang size to 9."])

applyLog :: Monoid m => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, oldLog) f = let (y, newLog) = f x in (y, oldLog `mappend` newLog)

checkAndLog :: (Bool, [String])
checkAndLog = applyLog (2, ["Small band"]) isBigGang

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _       = ("beer", Sum 30)

drinkToBeans :: (Food, Sum Int)
drinkToBeans = ("beans", Sum 15) `applyLog` addDrink

-- newtype Writer w a = Writer { runWriter :: (a, w) }

-- instance (Monoid w) => Monad (Writer w) where
--   return x = Writer (x, mempty)
--   (Writer (x, v)) >>= f =
--     let (Writer (y, v')) = f x in Writer (y, v `mappend` v')

strWriter :: Writer [String] Int
strWriter = return 3 :: Writer [String] Int -- Writer (3, [])

sumWriter :: (Int, Sum Int)
sumWriter = runWriter (return 3 :: Writer (Sum Int) Int) -- (3, Sum {getSum = 0})


-- Writer in do notation


logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
  a <- logNumber 3
  b <- logNumber 5
  tell ["Gonna multiply these two"] -- this will mappend to momoid value
  return (a * b) -- nothing gets logged, since we return and the last entry would be mempty

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
  | b == 0 = do
    tell ["Finished with " ++ show a]
    return a
  | otherwise = do
    let val = a `mod` b
    tell [show a ++ " mod " ++ show b ++ " = " ++ show val]
    gcd' b val

showLog :: IO ()
showLog = do
  putStrLn "here is the calculation of gcd' 3124 213"
  mapM_ putStrLn $ execWriter $ gcd' 3124 213

-- difference lists

-- f = ("dog" ++)
-- g = ("meat" ++)
-- f `append` g = \xs -> "dog" ++ ("meat" ++ xs)
append :: (t1 -> t2) -> (t3 -> t1) -> t3 -> t2
f `append` g = \xs -> f (g xs)


newtype DiffList a = DiffList { getDiffList :: [a] -> [a]}

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Semigroup (DiffList a) where
  (DiffList f) <> (DiffList g) = DiffList (\xs -> f (g xs))

instance Monoid (DiffList a) where
  mempty = DiffList (\xs -> [] ++ xs)
  (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

ifWorks :: [Integer]
ifWorks = fromDiffList (toDiffList [1, 2, 3] `mappend` toDiffList [4, 5, 6])


gcd'' :: Int -> Int -> Writer (DiffList String) Int
gcd'' a b
  | b == 0 = do
    tell (toDiffList ["Finished with " ++ show a])
    return a
  | otherwise = do
    let val = a `mod` b
    result <- gcd'' b val
    tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show val])
    return result

gcdResult :: IO ()
gcdResult = mapM_ putStrLn . fromDiffList . snd . runWriter $ gcd'' 110 34
-- Finished with 2
-- 8 mod 2 = 0
-- 34 mod 8 = 2
-- 110 mod 34 = 8
