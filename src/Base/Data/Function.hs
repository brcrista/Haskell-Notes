{-# LANGUAGE NoImplicitPrelude #-}

module Base.Data.Function where

id :: a -> a
id x = x

const :: a -> b -> a
const x _ = x

flip :: (a -> b -> c) -> (b -> a -> c)
flip f y x = f x y

(.) :: (b -> c) -> (a -> b) -> (a -> c)
(f . g) x = f (g x)

($) :: (a -> b) -> a -> b
f $ x = f x

(&) :: a -> (a -> b) -> b
x & f = f x

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(f `on` g) x y = f (g x) (g y)

-- TODO `fix`