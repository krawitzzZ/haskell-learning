module Monads.State where

import           System.Random
import           Control.Monad
import           Control.Monad.State


type Stack = [Int]

-- pop :: Stack -> (Int, Stack)
-- pop []       = (0, [])
-- pop (x : xs) = (x, xs)

-- push :: Int -> Stack -> ((), Stack)
-- push x xs = ((), x : xs)

-- there is a type for stateful computations in Control.Monad.State:
-- newtype State s a = State { runState :: s -> (a,s) }
-- instance Monad (State s) where
--   return x = State $ \s -> (x, s)
--   (State h) >>= f = State $ \s ->
--     let (a, newState) = h s
--         (State g)     = f a
--     in  g newState

-- let's rewrite pop and push

pop :: State Stack Int
pop = state $ \(x : xs) -> (x, xs)

push :: Int -> State Stack ()
-- push a = state $ \xs -> ((), a : xs)
push a = state $ \xs -> ((), a : xs)

stackManip :: State Stack Int
stackManip = do
  _ <- pop
  _ <- pop
  push 3
  push 100
  pop

stackStuff :: State Stack ()
stackStuff = do
  a <- pop
  if a == 5
    then push 5
    else do
      push 3
      push 8

moreStack :: State Stack ()
moreStack = do
  a <- stackManip
--   if a == 100 then stackStuff else return ()
  when (a == 100) stackStuff

stackyStack :: State Stack ()
stackyStack = do
  stackNow <- get
  if stackNow == [1, 2, 3] then put [8, 3, 1] else put [9, 2, 1]

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
  a <- randomSt
  b <- randomSt
  c <- randomSt
  return (a, b, c)

coins :: (Bool, Bool, Bool)
coins = evalState threeCoins (mkStdGen 23)
