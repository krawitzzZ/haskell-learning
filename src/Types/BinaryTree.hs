module Types.BinaryTree where


import           Prelude                 hiding ( elem )

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show, Read, Eq, Ord)

leaf :: Tree a
leaf = Leaf

singleton :: a -> Tree a
singleton a = Node a Leaf Leaf

insert :: Ord a => a -> Tree a -> Tree a
insert v Leaf = singleton v
insert v (Node x left right) | v < x     = Node x (insert v left) right
                             | v > x     = Node x left (insert v right)
                             | v == x    = Node x left right
                             | otherwise = Node x left right

elem :: Ord a => a -> Tree a -> Bool
elem _ Leaf = False
elem v (Node x left right) | v < x     = elem v left
                           | v > x     = elem v right
                           | v == x    = True
                           | otherwise = False

nums :: [Integer]
nums = [8, 6, 4, 1, 7, 3, 5]

tree :: Tree Integer
tree = foldr insert leaf nums
