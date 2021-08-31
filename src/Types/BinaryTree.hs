module Types.BinaryTree where


import           Prelude                 hiding ( elem )
import qualified Data.Foldable                 as F

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq, Ord)

instance Functor Tree  where
  fmap _ EmptyTree    = EmptyTree
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

instance Foldable Tree where
  foldMap _ EmptyTree    = mempty
  foldMap f (Node x l r) = F.foldMap f l <> f x <> F.foldMap f r -- <> = `mappend`


leaf :: Tree a
leaf = EmptyTree

singleton :: a -> Tree a
singleton a = Node a EmptyTree EmptyTree

insert :: Ord a => a -> Tree a -> Tree a
insert v EmptyTree = singleton v
insert v (Node x left right) | v < x     = Node x (insert v left) right
                             | v > x     = Node x left (insert v right)
                             | v == x    = Node x left right
                             | otherwise = Node x left right

elem :: Ord a => a -> Tree a -> Bool
elem _ EmptyTree = False
elem v (Node x left right) | v < x     = elem v left
                           | v > x     = elem v right
                           | v == x    = True
                           | otherwise = False

nums :: [Integer]
nums = [8, 6, 4, 1, 7, 3, 5]

tree :: Tree Integer
tree = foldr insert leaf nums
