module Lists where

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

listsMain :: IO ()
listsMain = do
  putStrLn "Lists:"
  putStrLn $ "eftChar: " ++ eftChar 'a' 'z'
  putStrLn $ "eftInt: " ++ show (eftInt 4 23)
  putStrLn $ "splitOn ' ': " ++ show (splitOn ' ' "one two three")
  putStrLn $ "splitOn '\\n': " ++ show (splitOn '\n' "one\ntwo\nthree")
  putStrLn $ "squares from comprehension: " ++ show sqr
  putStrLn $ "cubes from comprehension: " ++ show cub
  putStrLn $ "tuples from comprehension: " ++ show tuples
  putStrLn $ "tuples where x and y < 50: " ++ show tuplesLessThan
  putStrLn
    $  "amount of tuples where x and y < 50: "
    ++ show amountOfTuplesLessThan
  putStrLn $ "myAnd for [True, True]: " ++ show (myAnd [True, True])
  putStrLn $ "myAnd for [True, False]: " ++ show (myAnd [True, False])
  putStrLn $ "myOr for [False, True]: " ++ show (myOr [False, True])
  putStrLn $ "myOr for [False, False]: " ++ show (myOr [False, False])
  putStrLn $ "myAny for even [1, 3, 5]: " ++ show
    (myAny even ([1, 3, 5] :: [Integer]))
  putStrLn $ "myAny for odd [1, 3, 5]: " ++ show
    (myAny odd ([1, 3, 5] :: [Integer]))
  putStrLn $ "myElem for 3 [1, 3, 5]: " ++ show
    (myElem (3 :: Integer) [1, 3, 5])
  putStrLn $ "myElem for 2 [1, 3, 5]: " ++ show
    (myElem (2 :: Integer) [1, 3, 5])
  putStrLn $ "myReverse for [1..5]: " ++ show
    (myReverse ([1 .. 5] :: [Integer]))
  putStrLn $ "squish for [[1,2], [3,4]]: " ++ show
    (squish ([[1, 2], [3, 4]] :: [[Integer]]))
  putStrLn $ "squishMap for (\\x -> 'WO ' ++ [x] ++ ' HOO ') '123': " ++ show
    (squishMap (\x -> "WO " ++ [x] ++ " HOO ") "123")
  putStrLn $ "squishAgain for [[1,2], [3,4]]: " ++ show
    (squishAgain ([[1, 2], [3, 4]] :: [[Integer]]))
  putStrLn $ "myMaximumBy for compare [1, 15, 209, 5]: " ++ show
    (myMaximumBy compare ([1, 15, 209, 5] :: [Integer]))
  putStrLn $ "myMinimumBy for compare [1, 15, 209, 5]: " ++ show
    (myMinimumBy compare ([1, 15, 209, 5] :: [Integer]))
  putStrLn $ "myMaximum for [1, 15, 209, 5]: " ++ show
    (myMaximum ([1, 15, 209, 5] :: [Integer]))
  putStrLn $ "myMinimum for [1, 15, 209, 5]: " ++ show
    (myMinimum ([1, 15, 209, 5] :: [Integer]))
  putStrLn "\n"
