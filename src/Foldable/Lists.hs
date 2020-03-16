module Foldable.Lists where

-- list comprehension
-- [x^y | x <- [1..5], y <- [2..10], rem x 2 == 0]

-- enumFromTo for Char
eftChar :: Char -> Char -> [Char]
eftChar = eft []

-- enumFromTo for Int
eftInt :: Int -> Int -> [Int]
eftInt = eft []

-- enumFromTo
eft :: (Enum a, Eq a) => [a] -> a -> a -> [a]
eft list from to | from == to = list ++ [to]
                 | otherwise  = eft (list ++ [from]) (succ from) to

-- split a string to a list using separator
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn sep (x : xs) | null xs   = []
                     | x == sep  = toList xs
                     | otherwise = toList (x : xs)
  where toList ys = takeWhile (/= sep) ys : splitOn sep (dropWhile (/= sep) ys)

sqr :: [Integer]
sqr = [ x ^ (2 :: Integer) | x <- [1 .. 4] ]

cub :: [Integer]
cub = [ y ^ (3 :: Integer) | y <- [1 .. 4] ]

tuples :: [(Integer, Integer)]
tuples = [ (x, y) | x <- sqr, y <- cub ]

tuplesLessThan :: [(Integer, Integer)]
tuplesLessThan = [ (x, y) | x <- sqr, y <- cub, x < 50 && y < 50 ]

amountOfTuplesLessThan :: Int
amountOfTuplesLessThan = length tuplesLessThan


myAnd :: [Bool] -> Bool
-- myAnd []       = True
-- myAnd (x : xs) = x && myAnd xs
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
-- myOr []       = False
-- myOr (x : xs) = x || myAnd xs
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
-- myAny f []       = False
-- myAny f (x : xs) = f x || myAny f xs
myAny f = foldr ((||) . f) False

myElem :: Eq a => a -> [a] -> Bool
-- myElem el []       = False
-- myElem el (x : xs) = el == x || myElem el xs
myElem el = foldr (\x -> (||) (el == x)) False

myReverse :: [a] -> [a]
myReverse []       = []
myReverse (x : xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ []  = error "empty list"
myMaximumBy _ [x] = x
myMaximumBy f (x : y : ls) | f x y == GT = myMaximumBy f (x : ls)
                           | otherwise   = myMaximumBy f (y : ls)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ []  = error "empty list"
myMinimumBy _ [x] = x
myMinimumBy f (x : y : ls) | f x y == LT = myMinimumBy f (x : ls)
                           | otherwise   = myMinimumBy f (y : ls)

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
