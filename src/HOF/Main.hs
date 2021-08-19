module HOF.Main where


-- partial application and curring

-- multThree :: Num a => a -> (a -> (a -> a))
multThree :: Num a => a -> a -> a -> a
multThree x y z = x * y * z

-- multTwoWithTen :: Num a => a -> (a -> a)
multTwoWithTen :: Num a => a -> a -> a
-- multTwoWithTen x y = multThree 10 x y
multTwoWithTen = multThree 10

compareWithHundred :: (Num a, Ord a) => a -> Ordering
-- compareWithHundred x = compare 100 x
compareWithHundred = compare 100

divideByTen :: Floating a => a -> a
-- divideByTen x = x / 10
divideByTen = (/ 10)

tenDevidedBy :: Floating a => a -> a
-- tenDevidedBy x = 10 / x
tenDevidedBy = (10 /)

isUpperAlphaNum :: Char -> Bool
-- isUpperAlphaNum x = x `elem` ['A' .. 'Z']
isUpperAlphaNum = (`elem` ['A' .. 'Z'])


-- funcs as arguments and return values

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ []       _        = []
zipWith' _ _        []       = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys


-- quicksort with filter

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerSorted = quicksort $ filter (< x) xs
      biggerSorted  = quicksort $ filter (>= x) xs
  in  smallerSorted ++ [x] ++ biggerSorted

largestDevisible :: Integral a => a -> a -> a
largestDevisible num devisor = last $ filter predicate [0 .. num]
  where predicate x = x `mod` devisor == 0

sumOddSquaredLessThan :: Integral a => a -> a
-- sumOddSquaredLessThan x = sum [ n ^ (2 :: Integer) | n <- [1 .. x], odd n ]
-- sumOddSquaredLessThan x = sum (map (^ (2 :: Integer)) (filter odd [1 .. x]))
sumOddSquaredLessThan x = sum $ map (^ (2 :: Integer)) $ filter odd [1 .. x]

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n | even n    = n : chain (n `div` 2)
        | odd n     = n : chain (n * 3 + 1)
        | otherwise = error "oops"

chainLengthLessThan :: Integral a => Int -> [[a]]
chainLengthLessThan x = filter (\xs -> length xs <= x) $ map chain [1 .. 100]


-- folds

elem' :: Eq a => a -> [a] -> Bool
elem' x xs = foldl (\acc el -> if el == x then True else acc) False xs

sum' :: Num a => [a] -> a
-- sum' xs = foldl (\acc curr -> acc + curr) 0 xs
-- sum' = foldl (+) 0
sum' = foldl1 (+)

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

maximum' :: Ord a => [a] -> a
maximum' = foldl1 (\x y -> if x > y then x else y)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: Num a => [a] -> a
product' = foldl1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x acc -> if f x then x : acc else acc) []


-- scans // the same as folds but keep the progress in form of a list

sumSqrtLengthLessThen :: Float -> Int
sumSqrtLengthLessThen x =
  length (takeWhile (< x) (scanl1 (+) (map sqrt [1 ..]))) + 1


-- function composition

negateAll :: Num a => [a] -> [a]
-- negateAll xs = map (\x -> negate (abs x)) xs
negateAll = map (negate . abs)

getDatSum :: Int -> Int -> Int -> Int
-- getDatSum x y z = sum (replicate z (max x y))
getDatSum x y z = sum . replicate z . max x $ y


toApplication :: Int -> [Integer]
-- toApplication = replicate 100 (product (map (*3) (zipWith max [1,2,3,4,5] [4,5,6,7,8])))
toApplication x =
  replicate x
    . product
    . map (* 3)
    . zipWith max [1, 2, 3, 4, 5]
    $ [4, 5, 6, 7, 8]

anotherApplication :: (RealFrac a, Floating a, Integral b) => a -> a -> b
-- put last argument with function application: $, and then remove parentheses by using function composition: .
-- anotherApplication x y = ceiling (negate (tan (cos (max x y))))
-- anotherApplication x y = ceiling . negate . tan . cos . max x $ y
anotherApplication x = ceiling . negate . tan . cos . max x

oddSquareSum :: Integer
-- oddSquareSum = sum (takeWhile (< 10000) (filter odd (map (^ 2) [1 ..])))
-- oddSquareSum = sum . takeWhile (< 10000) . filter odd . map (^ 2) $ [1 ..]
oddSquareSum =
  let oddSquares        = filter odd $ map (^ 2) [1 ..]
      lessThanAthousand = takeWhile (< 1000) oddSquares
  in  sum lessThanAthousand
