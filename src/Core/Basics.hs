module Core.Basics where

import           Data.List                      ( intercalate )

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


type Name = String

type Age = Integer

data Person = Person Name Age deriving (Show, Eq, Ord)

sum' :: Integer -> Integer
sum' 0    = 0
sum' 1    = 1
sum' (-1) = -1
sum' x    = x + sum' next where next = if x > 0 then x - 1 else x + 1

product' :: Integer -> Integer -> Integer
product' x y = go x y 0 0
 where
  go m1 m2 result count
    | m1 == 0 || m2 == 0 = 0
    | m1 == 1            = m2
    | m2 == 1            = m1
    | m1 == -1           = -m2
    | m2 == -1           = -m1
    | count == abs m2    = result
    | otherwise          = go m1 m2 (getResult result m1 m2) (count + 1)
  getResult r m1' m2' = if m2' > 0 then r + m1' else r - m1'

data DividedResult a = DividedResult (a, a) | DividedByZero deriving (Show, Eq)

division :: Integer -> Integer -> DividedResult Integer
division num denom = go num denom 0 isPositive where
  isPositive = num > 0 && denom > 0 || num < 0 && denom < 0
  go n d count isPositiveResult
    | d == 0 = DividedByZero
    | abs n < abs d = DividedResult
      (if isPositiveResult then count else (-count), n)
    | otherwise = go (abs n - abs d) d (count + 1) isPositiveResult


mc91 :: Integral a => a -> a
mc91 x | x > 100   = x - 10
       | x <= 100  = mc91 $ mc91 $ x + 11
       | otherwise = x


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
