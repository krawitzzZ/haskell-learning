module Functors.Monoids where

import           Data.Monoid
import qualified Data.Foldable                 as F
import qualified Types.BinaryTree              as T


-- mappend = <>
-- monoid laws
-- 1) mempty `mappend` x = x
-- 2) x `mappend` mempty = x
-- 3) (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)


-- class Monoid m where
--   mempty :: m
--   mappend :: m -> m -> m
--   mconcat :: [m] -> m
--   mconcat = foldr mappend mempty

-- instance Monoid [a] where
--   mempty  = []
--   mappend = (++)
--   mconcat = concat

prods :: Integer
prods = getProduct . mconcat . map Product $ [1, 4, 6]

sums :: Integer
sums = getSum . mconcat . map Sum $ [1, 4, 6]

anys :: Bool
anys = getAny . mconcat . map Any $ [False, True, False]

alls :: Bool
alls = getAll . mconcat . map All $ [True, False, True]

-- instance Monoid Ordering where
--   mempty = EQ
--   LT `mappend` _ = LT
--   EQ `mappend` y = y
--   GT `mappend` _ = GT

lengthCompare :: String -> String -> Ordering
lengthCompare x y =
  (length x `compare` length y)
    `mappend` (vowels x `compare` vowels y)
    `mappend` (x `compare` y)
  where vowels = length . filter (`elem` "aeiou")

-- instance Monoid a => Monoid (Maybe a) where
--   mempty = Nothing
--   Nothing `mappend` m       = m
--   m       `mappend` Nothing = m
--   Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)

justMonoid :: Maybe Integer
justMonoid = getSum <$> Just (Sum 3) `mappend` Just (Sum 4) -- Just 7

-- instance Monoid (First a) where
--   mempty = First Nothing
--   First (Just x) `mappend` _ = First (Just x)
--   First Nothing  `mappend` x = x

firstMonoid :: Maybe Integer
firstMonoid = getFirst $ First (Just 1) `mappend` First (Just 2) -- Just 1

firstMonoid' :: Maybe Integer
firstMonoid' = getFirst . mconcat . map First $ [Nothing, Just 9, Just 10] -- Just 9

lastMonoid :: Maybe Integer
lastMonoid = getLast $ Last (Just 1) `mappend` Last (Just 2) -- Just 2


-- Foldable

justFolded :: Integer
justFolded = F.foldl (+) 2 (Just 8)

testTree :: T.Tree Integer
testTree = T.Node
  5
  (T.Node 3
          (T.Node 1 T.EmptyTree T.EmptyTree)
          (T.Node 6 T.EmptyTree T.EmptyTree)
  )
  (T.Node 9
          (T.Node 8 T.EmptyTree T.EmptyTree)
          (T.Node 10 T.EmptyTree T.EmptyTree)
  )

foldedTree :: Integer
foldedTree = F.foldr (+) 0 testTree

foldedTree' :: Integer
foldedTree' = F.foldr (*) 1 testTree

hasThree :: Bool
hasThree = getAny $ F.foldMap (\x -> Any $ x == 3) testTree
