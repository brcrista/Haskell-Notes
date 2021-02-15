{-# LANGUAGE NoImplicitPrelude #-}

module Base.Data.Function where

id :: a -> a
id x = x

const :: a -> b -> a
const x _ = x

flip :: (a -> b -> c) -> (b -> a -> c)
flip f y x = f x y

($) :: (a -> b) -> a -> b
f $ x = f x

(.) :: (b -> c) -> (a -> b) -> (a -> c)
(f . g) x = f (g x)

-- TODO `on`
-- TODO `fix`