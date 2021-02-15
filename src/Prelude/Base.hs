{-# LANGUAGE NoImplicitPrelude #-}

module Prelude.Base where

import GHC.Types (Char)

data Bool = False | True -- deriving (Eq, Ord, Show)

(&&) :: Bool -> Bool -> Bool
True && True = True
_    && _    = False

not :: Bool -> Bool
not False = True
not True = False

type String = [Char]

id :: a -> a
id x = x

const :: a -> b -> a
const x _ = x

flip :: (a -> b -> c) -> (b -> a -> c)
flip f y x = f x y

-- error :: String -> a

otherwise :: Bool
otherwise = True

-- compare :: Ord a => a -> a -> Ordering
-- compare x y
--   | x < y = LT
--   | x > y = GT
--   | otherwise = EQ

-- max :: Ord a => a -> a -> a
-- max x y
--   | x > y = x
--   | otherwise = y

-- min :: Ord a => a -> a -> a
-- min x y
--   | x < y = x
--   | otherwise = y



