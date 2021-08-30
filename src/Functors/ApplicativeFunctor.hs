module Functors.ApplicativeFunctor where

import           Control.Applicative
import           Prelude                 hiding ( sequenceA )


-- applicative functor laws
-- 1) pure f <*> x = fmap f x
-- 2) pure id <*> v = v
-- 3) pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- 4) pure f <*> pure x = pure (f x)
-- 5) u <*> pure y = pure ($ y) <*> u



-- sequential application: <*> - takes a func in a context and applies it to a value in the same context:
-- `Just (1+) <*> Just (1) = Just 2`

-- infix fmap: <$> - takes a func and a value in a context and applies the func to the value:
-- `(+) <$> Just 1 = Just (1+)`
-- which is the same as: `pure (+) <*> Just 1 = Just (1+)`

-- Applicative functor
data Probably a = Nope | Exactly a deriving (Show)

-- functor instance
instance Functor Probably where
  fmap _ Nope        = Nope
  fmap f (Exactly a) = Exactly (f a)

-- <*> stands for sequential application
instance Applicative Probably where
  pure = Exactly
-- which is equivalent to
--   pure x = Exactly x
  Nope               <*> _       = Nope
  (Exactly function) <*> functor = fmap function functor
-- which is equivalent to
--   (<*>) Nope               _       = Nope
--   (<*>) (Exactly function) functor = fmap function functor

data OneOrAnother a b = One a | Another b deriving (Show, Eq, Ord)

instance Functor (OneOrAnother a) where
  fmap _ (One     a) = One a
  fmap f (Another b) = Another (f b)

instance Applicative (OneOrAnother a) where
  pure = Another
  (One     a       ) <*> _       = One a
  (Another function) <*> functor = fmap function functor

infixFMap :: Maybe String
infixFMap = (++) <$> Just "John " <*> Just "Doe"

seqApplOnList :: [Integer]
seqApplOnList = [(+ 1), (* 2), (^ (2 :: Integer))] <*> [1, 2, 3]

seqApplOnLists :: [Integer]
seqApplOnLists = [(+), (*)] <*> [1, 2] <*> [1, 2]

seqApplOnLists' :: [String]
seqApplOnLists' = (++) <$> ["ha", "heh", "hmm"] <*> ["?", ".", "!"]

-- list comprehensions can be substituted with applicative functions:
-- `[x * y | x <- [2, 5, 10], y <- [8, 10, 11]] == (*) <$> [2, 5, 10] <*> [8, 10, 11]`


-- some applicative stuff with IO

myAction :: IO String
myAction = do
  one <- getLine
  two <- getLine
  return $ one ++ two

-- which is the same as
myAction' :: IO String
myAction' = (++) <$> getLine <*> getLine

funcAsApplicative :: [Double]
funcAsApplicative = (\x y z -> [x, y, z]) <$> (+ 3) <*> (* 2) <*> (/ 2) $ 5
{-
  1) (\x y z -> [x, y, z]) <$> (+ 3) :: Num a => a -> a -> a -> [a]
  2) (\x y z -> [x, y, z]) <$> (+ 3) <*> (* 2) :: Num a => a -> a -> [a]
  3) (\x y z -> [x, y, z]) <$> (+ 3) <*> (* 2) <*> (/ 2) :: Fractional a => a -> [a]
  4) (\x y z -> [x, y, z]) <$> (+ 3) <*> (* 2) <*> (/ 2) $ 5 :: Fractional t => [t] which is [8.0,10.0,2.5]
-}

-- When we do `(+) <$> (+10) <*> (+5)`, we're using `+` on the future return values of `(+10)` and `(+5)`
-- and the result is also something that will produce a value only when called with a parameter

-- ZipList
-- it is an applicative the way one would expect it:
-- [(+ 1), (* 3)] <*> [2, 4] = [2 + 1, 4 * 3] = [3, 12]
zipList :: [Integer]
zipList = getZipList $ (+) <$> ZipList [1, 2, 3, 4] <*> ZipList [100, 100, 100]

-- here we can zip as much lists as we need
zipList' :: [(Char, Char, Char, Char)]
zipList' =
  getZipList
    $   (,,,)
    <$> ZipList "dog"
    <*> ZipList "cat"
    <*> ZipList "rat"
    <*> ZipList "map"

-- liftA2

liftedExample :: Maybe [Integer]
liftedExample = liftA2 (:) (Just 4) (Just [5])

liftedExample' :: [Integer]
liftedExample' = liftA2 (*) [1, 2, 3] [4, 5, 6]

sequenceA :: (Applicative f) => [f a] -> f [a]
-- sequenceA [] = pure []
-- sequenceA (x : xs) = (:) <$> x <*> sequenceA xs
-- or
-- sequenceA xs = foldr (\x acc -> ((:) <$> x) <*> acc) (pure []) xs
-- sequenceA xs = foldr (\x -> (<*>) ((:) <$> x)) (pure []) xs
-- or
-- sequenceA xs = foldr (\x acc -> liftA2 (:) x acc) (pure []) xs
sequenceA xs = foldr (liftA2 (:)) (pure []) xs

mySeq :: Maybe [Integer]
mySeq = sequenceA [Just 1, Just 2, Just 3, Just 12]

mySeq' :: [Integer]
mySeq' = sequenceA [(+ 5), (* 3), (+ 20)] 5

isAllTrue :: Bool
isAllTrue = and $ map (\f -> f 7) [(> 4), (< 10), odd]

isAllTrue' :: Bool
isAllTrue' = and $ sequenceA [(> 4), (< 10), odd] 7
