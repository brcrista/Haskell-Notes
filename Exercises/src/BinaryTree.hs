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

toMaybe :: BinaryTree a -> Maybe a
toMaybe Nil = Nothing
toMaybe (Node x _ _) = Just x

subtreeLeft :: BinaryTree a -> BinaryTree a
subtreeLeft Nil = Nil
subtreeLeft (Node _ left _) = left

subtreeRight :: BinaryTree a -> BinaryTree a
subtreeRight Nil = Nil
subtreeRight (Node _ _ right) = right

-- | The number of levels in the tree.
height :: BinaryTree a -> Int
height Nil = 0
height (Node _ left right) = 1 + max (length left) (length right)