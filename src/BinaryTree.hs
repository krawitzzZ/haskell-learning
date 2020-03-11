module BinaryTree
  ( binaryTreeMain
  )
where

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

insert' :: (Ord a) => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right) | b == a = Node left a right
                              | b < a  = Node (insert' b left) a right
                              | b > a  = Node left a (insert' b right)

asd = foldl (flip insert') Leaf [8, 9, 12, 5, 1, 20, 4]

binaryTreeMain :: IO ()
binaryTreeMain = do
  putStrLn "BinaryTree:"
  print asd
  putStrLn "\n"
