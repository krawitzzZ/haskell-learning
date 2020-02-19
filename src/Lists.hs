module Lists
  ( listsMain
  )
where

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

sqr = [ x ^ 2 | x <- [1 .. 4] ]
cub = [ y ^ 3 | y <- [1 .. 4] ]
tuples = [ (x, y) | x <- sqr, y <- cub ]
tuplesLessThan = [ (x, y) | x <- sqr, y <- cub, x < 50 && y < 50 ]
amountOfTuplesLessThan = length tuplesLessThan

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
  putStrLn "\n"
