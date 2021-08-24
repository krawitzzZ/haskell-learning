module Modules.List where

-- import           Data.List -- import the whole module
-- import           Data.List hiding ( sort ) -- import the whole module but `sort`
-- import qualified Data.List -- import module, but for using it one would need to prepend function call with `Data.List`
-- import qualified Data.List as L -- the same as above, but now we can prepend only `L` (named import)
-- import           Data.List                      ( nub ) -- import only `nub` function

import qualified Data.List                     as List
import           Data.Function                  ( on )


numUnieques :: Eq a => [a] -> Int
numUnieques = length . List.nub

-- put an element in between every element in array
intersperse' :: String
intersperse' = List.intersperse '.' "awesome"

 -- takes a list, a list of lists, puts first list between all the lists inside and flattens the list of lists
intercalate' :: String
intercalate' = List.intercalate " " ["that", "is", "awesome"]

-- flattens the list
concat' :: [Integer]
concat' = List.concat [[1], [2], [3], [4]]

-- first maps and then flattens the list
concatMap' :: [Integer]
concatMap' = List.concatMap (replicate 3) [1 .. 5]

-- returns true if all elements in list are true
and' :: Bool
and' = List.and $ map (> 4) [1, 2, 3, 4, 5]

-- returns true if at least one element in list is true
or' :: Bool
or' = List.or $ map (== 5) [1, 2, 3, 4, 5]

-- any and all is return true if at least one element satisfies the predicate
any' :: Bool
any' = List.any (`elem` [1 .. 15]) [1, 2, 16]

-- applies the function to the starting value again and again. returns an infinite list
iterate' :: [Integer]
iterate' = take 5 $ List.iterate (* 5) 5

-- first list in the result contains all the elements that satisfy the predicate, the second contains all the ones that don't
partition' :: ([Integer], [Integer])
partition' = List.partition (< 4) [1, 2, 3, 4, 5, 6, 7, 1]

-- returns `Maybe value`
find' :: Maybe Integer
find' = List.find (`elem` [1 .. 3]) [1, 2, 3, 4, 5, 6, 7]

-- removes elements in the right list from the left list
doubleBackSlash :: String
doubleBackSlash = "I am just a guy" List.\\ " just"

-- inserts an element right at index where it's <= of the next element (at first occurance of such element)
insert' :: [Integer]
insert' = List.insert 4 [1, 2, 3, 4, 5, 6]

insertSorted :: [Integer]
insertSorted = List.insert 4 $ List.sort [1, 3, 5, 38, 92, 47, 2, 1, 3, 23]

nums :: [Double]
nums =
  [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, -2.4, -14.5, 2.9, 2.3]
-- groups the list on predicate
groupBy' :: [[Double]]
-- groupBy' = List.groupBy (\x y -> (x > 0) == (y > 0)) nums
groupBy' = List.groupBy ((==) `on` (> 0)) nums

anotherNums :: [[Integer]]
anotherNums = [[5, 4, 5, 4, 4], [1, 2, 3], [3, 5, 4, 3], [], [2], [2, 2]]
sortedInts :: [[Integer]]
sortedInts = List.sortBy (compare `on` length) anotherNums


data Dude = Dude { name:: String, age :: Int } deriving (Eq, Show)
dudes :: [Dude]
dudes =
  [ Dude { name = "max", age = 25 }
  , Dude { name = "john", age = 20 }
  , Dude { name = "ted", age = 30 }
  ]

sortedDudes :: [Dude]
-- sortedDudes = List.sortBy (\a b -> compare (age a) (age b)) dudes
sortedDudes = List.sortBy
  (\Dude { age = age1 } Dude { age = age2 } -> compare age1 age2)
  dudes
