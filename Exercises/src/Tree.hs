module Tree
where

data Tree a = Empty | Tree a (Tree a) (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Tree x left right) = Tree (f x) (fmap f left) (fmap f right)

instance Foldable Tree where
  foldr _ acc Empty = acc
  foldr f acc (Tree x left right) = f x (foldr f (foldr f acc right) left)

singleton :: a -> Tree a
singleton x = Tree x Empty Empty

toMaybe :: Tree a -> Maybe a
toMaybe Empty = Nothing
toMaybe (Tree x _ _) = Just x

subtreeLeft :: Tree a -> Tree a
subtreeLeft Empty = Empty
subtreeLeft (Tree _ left _) = left

subtreeRight :: Tree a -> Tree a
subtreeRight Empty = Empty
subtreeRight (Tree _ _ right) = right

-- | The number of levels in a tree.
height :: Tree a -> Int
height Empty = 0
height (Tree _ left right) = 1 + max (length left) (length right)

-- | Flip a tree on a vertical axis such that all left subtrees become right subtrees.
mirror :: Tree a -> Tree a
mirror Empty = Empty
mirror (Tree x left right) = Tree x (mirror right) (mirror left)

-- | Make a new tree with the same elements and `subtreeLeft` as the root.
rotateLeft :: Tree a -> Tree a
rotateLeft Empty = Empty
rotateLeft tree@(Tree x left right)
  = case left of
    Empty -> tree
    Tree y outer inner -> Tree y outer (Tree x inner right)

-- | Make a new tree with the same elements and `subtreeRight` as the root.
rotateRight :: Tree a -> Tree a
rotateRight Empty = Empty
rotateRight tree@(Tree x left right)
  = case right of
    Empty -> tree
    Tree y inner outer -> Tree y (Tree x left inner) outer