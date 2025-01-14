module Tree
where

data Tree a = Nil | Node a (Tree a) (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap f Nil = Nil
  fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

instance Foldable Tree where
  foldr _ acc Nil = acc
  foldr f acc (Node x left right) = f x (foldr f (foldr f acc right) left)

toMaybe :: Tree a -> Maybe a
toMaybe Nil = Nothing
toMaybe (Node x _ _) = Just x

subtreeLeft :: Tree a -> Tree a
subtreeLeft Nil = Nil
subtreeLeft (Node _ left _) = left

subtreeRight :: Tree a -> Tree a
subtreeRight Nil = Nil
subtreeRight (Node _ _ right) = right

-- | The number of levels in a tree.
height :: Tree a -> Int
height Nil = 0
height (Node _ left right) = 1 + max (length left) (length right)

-- | Flip a tree on a vertical axis such that all left subtrees become right subtrees.
mirror :: Tree a -> Tree a
mirror Nil = Nil
mirror (Node x left right) = Node x (mirror right) (mirror left)

-- | Make a new tree with the same elements and `subtreeLeft` as the root.
rotateLeft :: Tree a -> Tree a
rotateLeft Nil = Nil
rotateLeft tree@(Node x left right)
  = case left of
    Nil -> tree
    Node y outer inner -> Node y outer (Node x inner right)

-- | Make a new tree with the same elements and `subtreeRight` as the root.
rotateRight :: Tree a -> Tree a
rotateRight Nil = Nil
rotateRight tree@(Node x left right)
  = case right of
    Nil -> tree
    Node y inner outer -> Node y (Node x left inner) outer