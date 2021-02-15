{-# LANGUAGE NoImplicitPrelude #-}

module Base.Data.Foldable where

import Base.Core.Classes (Ord, Eq((==)), max, min)
import Base.Data.Bool (Bool(..), not, (&&), (||))
import Base.Data.Function ((.), flip,)
import Base.Data.List (reverse, map)
import Base.Prelude.Base (error)
import GHC.Num (Num((+), (*)))
import GHC.Types (Int)

-- Duplicated from Base.Data.List
emptyListError = error "empty list"

-- Replace Foldable with list to get simpler definitions that are better for learning.
-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ acc [] = acc
foldl f acc (x : xs) = foldl f (f acc x) xs

-- foldl1 :: Foldable t => (a -> a -> a) -> t a -> a
foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 f [] = emptyListError
foldl1 f (x : xs) = foldl f x xs

-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
-- Test case: these expressions should terminate (they wont with `foldl`)
--   foldr (\x y -> x) 0 [1..]
--   take 10 $ foldr (:) [] [1..]
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ acc [] = acc
foldr f acc (x : xs) = f x (foldr f acc xs)

-- foldr1 :: Foldable t => (a -> a -> a) -> t a -> a
foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 f (x : xs) = foldr f x xs

scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl _ acc [] = [acc]
scanl f acc (x : xs) = acc : scanl f (f acc x) xs

scanl1 :: (a -> a -> a) -> [a] -> [a]
scanl1 f [] = emptyListError
scanl1 f (x : xs) = scanl f x xs

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr f acc xs = scanl (flip f) acc (reverse xs)

scanr1 :: (a -> a -> a) -> [a] -> [a]
scanr1 f xs = scanl1 (flip f) (reverse xs)

-- and :: Foldable t => t Bool -> Bool
and :: [Bool] -> Bool
and = foldl (&&) True

-- or :: Foldable t => t Bool -> Bool
or :: [Bool] -> Bool
or = foldl (||) False

-- sum :: (Num a, Foldable t) => t a -> a
sum :: Num a => [a] -> a
sum = foldl (+) 0

-- product :: (Num a, Foldable t) => t a -> a
product :: Num a => [a] -> a
product = foldl (*) 1

-- maximum :: (Ord a, Foldable t) => t a -> a
maximum :: Ord a => [a] -> a
maximum = foldl1 max

-- minimum :: (Ord a, Foldable t) => t a -> a
minimum :: Ord a => [a] -> a
minimum = foldl1 min

-- any :: Foldable t => (a -> Bool) -> t a -> Bool
any :: (a -> Bool) -> [a] -> Bool
any f = or . map f

-- all :: Foldable t => (a -> Bool) -> t a -> Bool
all :: (a -> Bool) -> [a] -> Bool
all f = and . map f

-- elem :: (Eq a, Foldable t) => a -> t a -> Bool
elem :: Eq a => a -> [a] -> Bool
elem x = any (== x)

-- notElem :: (Eq a, Foldable t) => a -> t a -> Bool
notElem :: Eq a => a -> [a] -> Bool
notElem x = not . elem x

-- length :: Foldable t => t a -> Int
length :: [a] -> Int
length = foldl plusOne 0
  where
    plusOne acc _ = acc + 1

-- null :: Foldable t => t a -> Bool
null :: [a] -> Bool
null [] = True
null xs = False

-- concat :: Foldable t => t [a] -> [a]
-- concatMap :: Foldable t => (a -> [b]) -> t [a] -> [b]