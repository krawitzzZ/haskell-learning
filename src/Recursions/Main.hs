module Recursions.Main where


-- recursion

maximum' :: Ord a => [a] -> a
maximum' []  = error "gimme a non-empty list, you dummy!"
maximum' [x] = x
maximum' (x : xs) | x > maxTail = x
                  | otherwise   = maxTail
  where maxTail = maximum' xs

maximum'' :: Ord a => [a] -> a
maximum'' []       = error "gimme a non-empty list, you dummy!"
maximum'' [x     ] = x
maximum'' (x : xs) = max x $ maximum'' xs

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' 1 y = [y]
replicate' x y = y : replicate' (x - 1) y

replicate'' :: (Num i, Ord i) => i -> a -> [a]
replicate'' 1 y = [y]
replicate'' x y | x <= 0    = []
                | otherwise = y : replicate'' (x - 1) y

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _ | n <= 0 = [] -- guard without otherwise will just fall through to the next pattern matching
take' _ []         = []
take' n (x : xs)   = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' []       = []
reverse' (x : xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a, b)]
zip' _        []       = []
zip' []       _        = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

elem' :: Eq a => a -> [a] -> Bool
elem' _ []       = False
elem' x (y : ys) = x == y || elem x ys


-- quick sort

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerSorted = quicksort [ a | a <- xs, a <= x ]
      biggerSorted  = quicksort [ a | a <- xs, a > x ]
  in  smallerSorted ++ [x] ++ biggerSorted
