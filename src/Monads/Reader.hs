module Monads.Reader where

-- instance Monad ((->) r) where
--   return x = \_ -> x
--   h >>= f = \w -> f (h w) w


-- so
-- f = (+) <$> (*2) <*> (+10)
-- is the same as
addStuff :: Int -> Int
addStuff = do
  a <- (* 2)
  b <- (+ 10)
  return (a + b)
