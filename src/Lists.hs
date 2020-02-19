module Lists where

-- enumFromTo for Char
eftChar :: Char -> Char -> [Char]
eftChar = eft []

-- enumFromTo for Int
eftInt :: Int -> Int -> [Int]
eftInt = eft []

-- enumFromTo
eft :: (Enum a, Eq a) => [a] -> a -> a -> [a]
eft list from to | from == to = list ++ [to]
                 | otherwise  = eft (list ++ [to]) (succ from) to

-- split a string to a list using separator
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn sep (x : xs) | null xs   = []
                     | x == sep  = toList xs
                     | otherwise = toList (x : xs)
  where toList ys = takeWhile (/= sep) ys : splitOn sep (dropWhile (/= sep) ys)
