module Basics
  ( writeToConsole
  )
where

import           Data.List                      ( intercalate )

writeToConsole :: IO ()
writeToConsole = putStrLn "Yaikes, it works!"

data Blah = Blah

data WeekDay = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show)

instance Eq WeekDay where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _   _   = False

data Date = Date WeekDay Integer deriving (Show)

instance Eq Date where
  (==) (Date weekDay day) (Date weekDay' day') =
    weekDay == weekDay' && day == day'

newtype Identity a = Identity a

instance Eq a => Eq (Identity a) where
  (==) (Identity a) (Identity a') = a == a'

newtype TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn num) (TisAn num') = num == num'

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two a b) (Two a' b') = a == a' && b == b'

data StringOrInt = TisAnInt Integer | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt   a) (TisAnInt   a') = a == a'
  (==) (TisAString a) (TisAString a') = a == a'
  (==) _              _               = False

data Pair a = Pair a a deriving (Show)

instance Eq a => Eq (Pair a) where
  (==) (Pair x y) (Pair x' y') = x == x' && y == y'

data Tuple a b = Tuple a b deriving (Show)

instance (Eq a, Eq b) =>  Eq (Tuple a b) where
  (==) (Tuple x y) (Tuple x' y') = x == x' && y == y'

data Which a = ThisOne a | ThatOne a

instance Eq a => Eq ( Which a ) where
  (==) (ThisOne x) (ThisOne x') = x == x'
  (==) (ThatOne x) (ThatOne x') = x == x'
  (==) _           _            = False


summ :: (Eq a, Num a) => a -> a
summ 0 = 0
summ 1 = 1
summ x = x + summ (x - 1)

multi :: (Integral a) => a -> a -> a
multi x y = go x y 0 0
 where
  go multiplier1 multiplier2 product count
    | multiplier1 == 0 || multiplier2 == 0 = 0
    | multiplier1 == 1                     = multiplier2
    | multiplier2 == 1                     = multiplier1
    | count == multiplier2                 = product
    | otherwise = go (product + multiplier1) multiplier1 multiplier2 (count + 1)

data DividedResult a = Result (a, a) | DividedByZero deriving (Show)

division :: (Integral a) => a -> a -> DividedResult a
division num denom = go num denom 0 isPositive where
  isPositive = num > 0 && denom > 0 || num < 0 && denom < 0
  go n d count isPositiveResult
    | d == 0        = DividedByZero
    | abs n < abs d = Result (if isPositiveResult then count else (-count), n)
    | otherwise     = go (abs n - abs d) d (count + 1) isPositiveResult


mc91 :: Integral a => a -> a
mc91 x | x > 100  = x - 10
       | x <= 100 = mc91 $ mc91 $ x + 11


digitToWord :: Int -> String
digitToWord n = intercalate "-" $ map wordNumber $ digits n

digits :: Int -> [Int]
digits = go []
 where
  go xs num | num == 0  = xs
            | otherwise = go (mod num 10 : xs) (div num 10)

wordNumber :: Int -> String
wordNumber 0 = "zero"
wordNumber 1 = "one"
wordNumber 2 = "two"
wordNumber 3 = "three"
wordNumber 4 = "four"
wordNumber 5 = "five"
wordNumber 6 = "six"
wordNumber 7 = "seven"
wordNumber 8 = "eight"
wordNumber 9 = "nine"
wordNumber _ = error "wrong number"
