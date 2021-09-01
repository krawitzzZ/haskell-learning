module Monads.Main where

import           Control.Monad                  ( guard )


-- (>>=) :: (Monad m) => m a -> (a -> m b) -> m b
-- so for Maybe it would be
-- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b


-- monad laws
-- 1) return x >>= f = f x  OR  f <=< return = f
-- 2) m >>= return = m  OR  return <=< f = f
-- 3) (m >>= f) >>= g = m >>= (\x -> f x >>= g)  OR  f <=< (g <=< h) = (f <=< g) <=< h

-- monad function composition
-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
-- (<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c


-- class Monad m where
--   return :: a -> m a

--   (>>=) :: m a -> (a -> m b) -> m b

--   (>>) :: m a -> m b -> m b
--   x >> y = x >>= \_ -> y

--   fail :: String -> m a
--   fail msg = error msg

-- instance Monad Maybe where
--   return x = Just x
--   Nothing >>= f = Nothing
--   Just x  >>= f = f x
--   fail _ = Nothing

m :: String -> Maybe Bool
m s = Just s >>= (\v -> return (if v == "!" then True else False))

type Birds = Int
type Pole = (Birds, Birds)

-- landLeft :: Birds -> Pole -> Pole
-- landLeft n (l, r) = (l + n, r)

-- landRight :: Birds -> Pole -> Pole
-- landRight n (l, r) = (l, r + n)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (l, r) | abs ((l + n) - r) < 4 = Just (l + n, r)
                  | otherwise             = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (l, r) | abs (l - (r + n)) < 4 = Just (l, r + n)
                   | otherwise             = Nothing

pole :: Maybe Pole
pole = return (0, 0) >>= landRight 2 >>= landLeft 2 >>= landRight 2

pole' :: Maybe Pole
pole' =
  return (0, 0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight
    (-2)

banana :: Pole -> Maybe Pole
banana _ = Nothing

bananaPole :: Maybe Pole
bananaPole = return (0, 0) >>= landLeft 1 >>= banana >>= landRight 1

bananaPole' :: Maybe Pole
bananaPole' = return (0, 0) >>= landLeft 1 >> Nothing >>= landRight 1


-- monad do notation

justBool :: Maybe Bool
justBool = Just [True, False, True] >>= (\x -> Just (length x > 3))

-- is the same as
justBool' :: Maybe Bool
justBool' = do
  arr <- Just [True, False, True]
  let isLonger = length arr > 3
  Just isLonger

-- rewritten example of pole
routine :: Maybe Pole
routine = do
  let start = (0, 0)
  first  <- landLeft 2 start
  second <- landRight 2 first
  latest <- landRight 2 second >>= landLeft 1 >>= landRight 2
  landLeft 1 latest

-- since every line in do is monadic value we can have the following
routine' :: Maybe Pole
routine' = do
  let start = (0, 0)
  first  <- landLeft 2 start
  _      <- Nothing -- or just Nothing
  second <- landRight 2 first
  landLeft 1 second


-- list monades

-- instance Monad [] where
--   return x = [x]
--   xs >>= f = concat (map f xs)
--   fail _ = []

mlist :: [Integer]
mlist = [1, 2, 3] >>= (\x -> [x, -x])

mlist' :: [(Integer, Char)]
mlist' = [1, 2, 3] >>= \n -> ['a', 'b', 'c'] >>= \ch -> return (n, ch)

mdolist :: [(Integer, Char)]
mdolist = do
  n  <- [1, 2, 3]
  ch <- ['a', 'b', 'c']
  return (n, ch)

-- monad plus

-- class Monad m => MonadPlus m where
--   mzero :: m a
--   mplus :: m a -> m a -> m a

-- guard :: (MonadPlus m) => Bool -> m ()
-- guard True  = return ()
-- guard False = mzero


-- if guard is called with True, then >> is called like `() >> value`, so value is returned
guarded :: [String]
guarded = guard ('a' > 'A') >> return "cool"

-- if guard is called with False, then >> is called like `mzero >> value` so fail value is returned
-- e.g. Nothing for Maybe, or [] for lists
guarded' :: [String]
guarded' = guard ('a' < 'A') >> return "cool"

flist :: [Int]
flist = [1 .. 100] >>= (\x -> guard ('7' `elem` show x) >> return x)

flist' :: [Int]
flist' = do
  x <- [1 .. 100]
  guard ('7' `elem` show x)
  return x
