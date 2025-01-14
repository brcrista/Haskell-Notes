module BinaryTree
where

data BinaryTree a = Nil | Node a (BinaryTree a) (BinaryTree a)
  deriving (Eq, Show)

instance Functor BinaryTree where
  fmap f Nil = Nil
  fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

instance Foldable BinaryTree where
  foldr _ acc Nil = acc
  foldr f acc (Node x left right) = f x (foldr f (foldr f acc right) left)