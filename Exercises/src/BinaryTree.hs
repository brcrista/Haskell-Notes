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

-- | The number of levels in a tree.
height :: BinaryTree a -> Int
height Nil = 0
height (Node _ left right) = 1 + max (length left) (length right)

-- | Flip a tree on a vertical axis such that all left subtrees become right subtrees.
mirror :: BinaryTree a -> BinaryTree a
mirror Nil = Nil
mirror (Node x left right) = Node x (mirror right) (mirror left)

-- | Make a new tree with the same elements and `subtreeLeft` as the root.
rotateLeft :: BinaryTree a -> BinaryTree a
rotateLeft Nil = Nil
rotateLeft tree@(Node x left right)
  = case left of
    Nil -> tree
    Node y outer inner -> Node y outer (Node x inner right)

-- | Make a new tree with the same elements and `subtreeRight` as the root.
rotateRight :: BinaryTree a -> BinaryTree a
rotateRight Nil = Nil
rotateRight tree@(Node x left right)
  = case right of
    Nil -> tree
    Node y inner outer -> Node y (Node x left inner) outer