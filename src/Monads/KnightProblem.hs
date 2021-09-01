module Monads.KnightProblem where

import qualified Control.Monad                 as M

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
  (c', r') <-
    [ (c + 2, r - 1)
    , (c + 2, r + 1)
    , (c - 2, r - 1)
    , (c - 2, r + 1)
    , (c + 1, r - 2)
    , (c + 1, r + 2)
    , (c - 1, r - 2)
    , (c - 1, r + 2)
    ]
  M.guard (c' `elem` [1 .. 8] && r' `elem` [1 .. 8])
  return (c', r')

in3 :: KnightPos -> [KnightPos]
-- in3 start = moveKnight start >>= moveKnight >>= moveKnight
in3 start = moveKnight M.<=< moveKnight M.<=< moveKnight $ start

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start
