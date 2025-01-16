module Tree
where

data Tree a = Empty | Tree a (Tree a) (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f Empty = Empty
  fmap f (Tree x left right) = Tree (f x) (fmap f left) (fmap f right)

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
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
-- | Identity: mirror . mirror = id
mirror :: Tree a -> Tree a
mirror Empty = Empty
mirror (Tree x left right) = Tree x (mirror right) (mirror left)

-- | Make a new tree with the same elements and `subtreeLeft` as the root.
-- | Identity: rotateLeft = mirror . rotateRight . mirror
rotateLeft :: Tree a -> Tree a
rotateLeft Empty = Empty
rotateLeft tree@(Tree x left right)
  = case left of
    Empty -> tree
    Tree y outer inner -> Tree y outer (Tree x inner right)

-- | Make a new tree with the same elements and `subtreeRight` as the root.
-- | Identity: rotateRight = mirror . rotateLeft . mirror
rotateRight :: Tree a -> Tree a
rotateRight Empty = Empty
rotateRight tree@(Tree x left right)
  = case right of
    Empty -> tree
    Tree y inner outer -> Tree y (Tree x left inner) outer

-- TODO the different walk orders could probably be different instances of `Foldable`.
-- | Visit tree nodes in breadth-first order.
type Queue a = [a]

push :: a -> Queue a -> Queue a
push = (:)

-- In base-4.19 this is `Data.List.usnoc`
pull :: Queue a -> Maybe (Queue a, a)
pull [] = Nothing
pull xs = Just (init xs, last xs)

breadthFirstWalk :: Show a => Tree a -> IO ()
breadthFirstWalk tree = breadthFirstWalkRecursive [tree | not (null tree)]

breadthFirstWalkRecursive :: Show a => Queue (Tree a) -> IO ()
breadthFirstWalkRecursive [] = pure ()
breadthFirstWalkRecursive queue = do
  let Just (queue', node) = pull queue
  let Tree x left right = node
  print x
  let queue'' = if null left then queue' else push left queue'
  let queue''' = if null right then queue'' else push right queue''
  breadthFirstWalkRecursive queue'''
