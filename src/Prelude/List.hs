{-# LANGUAGE NoImplicitPrelude #-}

module Prelude.List where

import GHC.Types (Int)
import Prelude.Base (Bool)

emptyListError = error "empty list"

head :: [a] -> a
head [] = emptyListError
head (x : _) = x

tail :: [a] -> [a]
tail [] = emptyListError
tail (_ : xs) = xs

init :: [a] -> [a]
init [] = emptyListError
init [x] = []
init (x : xs) = x : init xs

last :: [a] -> a
last [] = emptyListError
last [x] = x
last (x : xs) = last xs

repeat :: a -> [a]
repeat x = x : repeat x

replicate :: Int -> a -> [a]
replicate n x
  | n <= 0 = []
  | otherwise = x : replicate (n - 1) x

cycle :: [a] -> [a]
cycle [] = emptyListError
cycle xs = xs ++ cycle xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

-- iterate :: (a -> a) -> a -> [a]

take :: Int -> [a] -> [a]
take _ [] = []
take n (x : xs)
  | n < 1 = []
  | otherwise = x : take (n - 1) xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile f (x : xs)
  | f x = x : takeWhile f xs
  | otherwise = []

drop :: Int -> [a] -> [a]
drop _ [] = []
drop n all@(x : xs)
  | n < 1 = all
  | otherwise = drop (n - 1) xs

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile f all@(x : xs)
  | f x = dropWhile f xs
  | otherwise = all

-- lookup :: Eq a => a -> [(a, b)] -> Maybe b

-- break :: (a -> Bool) -> [a] -> ([a], [a])

-- span :: (a -> Bool) -> [a] -> ([a], [a])

-- splitAt :: Int -> [a] -> ([a], [a])

map :: (a -> b) -> [a] -> [b]
map f xs = [f x | x <- xs]

filter :: (a -> Bool) -> [a] -> [a]
filter f xs = [x | x <- xs, f x]

zip :: [a] -> [b] -> [(a, b)]
zip = zipWith (,)

zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3 = zipWith3 (,,)

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (x : xs) (y : ys) = f x y : zipWith f xs ys
zipWith _ _ _ = []

zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 f (x : xs) (y : ys) (z : zs) = f x y z : zipWith3 f xs ys zs
zipWith3 _ _ _ _ = []

unzip :: [(a, b)] -> ([a], [b])
unzip xs = (map fst xs, map snd xs)

unzip3 :: [(a, b, c)] -> ([a], [b], [c])
unzip3 xs = ([x | (x, _, _) <- xs], [y | (_, y, _) <- xs], [z | (_, _, z) <- xs])