module BinaryTree where

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

insert' :: (Ord a) => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right) | b == a    = Node left a right
                              | b < a     = Node (insert' b left) a right
                              | b > a     = Node left a (insert' b right)
                              | otherwise = Node left a right

tree :: BinaryTree Int
tree = foldl (flip insert') Leaf ([8, 12, 5, 1, 20, 4] :: [Int])

binaryTreeMain :: IO ()
binaryTreeMain = do
  putStrLn "BinaryTree:"
  print tree
  putStrLn "\n"
