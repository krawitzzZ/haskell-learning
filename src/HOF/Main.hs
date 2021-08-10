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
