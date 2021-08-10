module SyntaxInFunctions.Main where

import           GHC.Float                      ( RealFloat )


-- recursion

factorial :: Integral a => a -> a
factorial 0 = 1
factorial x = x * factorial (x - 1)

-- pattern matching

head' :: [a] -> a
head' []      = error "dude, provide a non-empty list!"
head' (x : _) = x

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

someList :: [(Integer, Integer)]
someList = [(1, 3), (4, 3), (2, 4), (5, 3), (5, 6), (3, 1)]

comprehensiveListOfSomeList :: [Integer]
comprehensiveListOfSomeList = [ x + y | (x, y) <- someList ]

length' :: [a] -> Integer
length' []       = 0
length' (_ : xs) = 1 + length' xs

firstLetter :: String -> String
firstLetter ""          = "it's just empty"
firstLetter str@(x : _) = "First letter of " ++ str ++ " is " ++ [x]

-- guards

tellBmi :: RealFloat a => a -> a -> String
tellBmi height weight
  | weight / (height ^ 2) < 18.5 = "you have to eat, skinny!"
  | weight / (height ^ 2) < 25   = "you probably look ok! bet you're ugly"
  | weight / (height ^ 2) < 30   = "loose some fat, fatty!"
  | otherwise                    = "congrats, you are a whale"

-- where clause
tellBmi' :: RealFloat a => a -> a -> String
tellBmi' height weight | bmi < 18.5 = "you have to eat, skinny!"
                       | bmi < 25   = "you probably look ok! bet you're ugly"
                       | bmi < 30   = "loose some fat, fatty!"
                       | otherwise  = "congrats, you are a whale"
  where bmi = weight / (height * height)


-- let expressions

cylinder :: RealFloat a => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea  = pi * r ^ 2
  in  sideArea + 2 * topArea

letTuple :: (Integer, String)
letTuple =
  ( let a = 100
        b = 200
        c = 300
    in  a * b * c
  , let foo = "Hey "
        bar = "there!"
    in  foo ++ bar
  )

calcBmis :: RealFloat a => [(a, a)] -> [a]
calcBmis xs = [ bmi | (w, h) <- xs, let bmi = w / h ^ 2 ]

-- if we omit _in_ part, variables defined in _let_ will be visible within a scope
calcFatBmis :: RealFloat a => [(a, a)] -> [a]
calcFatBmis xs = [ bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0 ]


-- case expressions

anotherHead :: [a] -> a
anotherHead []      = error "give me something"
anotherHead (x : _) = x
-- is the same as
anotherHead' :: [a] -> a
anotherHead' xs = case xs of
  []      -> error "give me something"
  (x : _) -> x

describeList :: [a] -> String
describeList xs = "list " ++ what xs
 where
  what []  = "is empty"
  what [_] = "is a singlgeton list"
  what _   = "is a longer list"
