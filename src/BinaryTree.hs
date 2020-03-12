module BinaryTree where

import           Data.List

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

insert' :: (Ord a) => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right) | b == a    = Node left a right
                              | b < a     = Node (insert' b left) a right
                              | b > a     = Node left a (insert' b right)
                              | otherwise = Node left a right

tree :: BinaryTree Int
tree = foldl (flip insert') Leaf ([8, 5, 1, 4] :: [Int])

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf                = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

mapExpected :: BinaryTree Integer
mapExpected = Node (Node Leaf 2 Leaf) 3 (Node Leaf 4 Leaf)

mapTest :: String
mapTest = if mapTree (+ 1) testTree == mapExpected
  then "test passed!"
  else "test failed!"

preorder :: BinaryTree a -> [a]
preorder tr = preorder' tr []
 where
  preorder' Leaf xs = xs
  preorder' (Node left a right) xs =
    let temp   = xs ++ [a]
        left'  = preorder' left []
        right' = preorder' right []
    in  temp ++ left' ++ right'

inorder :: (Ord a) => BinaryTree a -> [a]
inorder = sort . preorder

postorder :: BinaryTree a -> [a]
postorder = postorder' []
 where
  postorder' xs Leaf = xs
  postorder' xs (Node left a right) =
    let temp   = xs ++ [a]
        left'  = postorder' [] left
        right' = postorder' [] right
    in  left' ++ right' ++ temp

testPreorder :: String
testPreorder =
  if preorder testTree == [2, 1, 3] then "test passed!" else "test failed!"

testInorder :: String
testInorder =
  if inorder testTree == [1, 2, 3] then "test passed!" else "test failed!"

testPostorder :: String
testPostorder =
  if postorder testTree == [1, 3, 2] then "test passed!" else "test failed!"

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ acc Leaf = acc
foldTree f acc (Node left val right) =
  let curr   = f val acc
      left'  = foldTree f curr left
      right' = foldTree f left' right
  in  right'

binaryTreeMain :: IO ()
binaryTreeMain = do
  putStrLn "BinaryTree:"
  print $ "tree: " ++ show tree
  putStrLn $ "mapTreeTest: " ++ mapTest
  putStrLn $ "preorderTreeTest: " ++ testPreorder
  putStrLn $ "inorderTreeTest: " ++ testInorder
  putStrLn $ "postorderTreeTest: " ++ testPostorder
  putStrLn $ "foldTree for 'tree': " ++ show (foldTree (+) 0 tree)
  putStrLn "\n"
