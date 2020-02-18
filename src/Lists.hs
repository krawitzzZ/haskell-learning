module Lists where

eftChar :: Char -> Char -> [Char]
eftChar = eft []

eftInt :: Int -> Int -> [Int]
eftInt = eft []

eft :: (Enum a, Eq a) => [a] -> a -> a -> [a]
eft list from to | from == to = list ++ [to]
                 | otherwise  = eft (list ++ [to]) (succ from) to
