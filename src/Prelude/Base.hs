{-# LANGUAGE NoImplicitPrelude #-}

module Prelude.Base where

-- TODO minimize GHC imports
import GHC.Classes (Eq, Ord)
import GHC.Show (Show)
import GHC.Types (Char)

-- Trying to use a custom Bool causes some problems.
-- GHC.Num functions return GHC.Base.Bool,
-- and guards depend on a LHS evaluating to GHC.Base.Bool.
-- data Bool = False | True deriving (Eq, Ord, Show)

-- not :: Bool -> Bool
-- not False = True
-- not True  = False

-- (&&) :: Bool -> Bool -> Bool
-- True && True = True
-- _    && _    = False

-- (||) :: Bool -> Bool -> Bool
-- False || False = False
-- _     || _     = True

-- otherwise :: Bool
-- otherwise = True

-- This would be the definition of [a] if it were valid Haskell:
-- data [a] = [] | a : [a]

type String = [Char]

id :: a -> a
id x = x

const :: a -> b -> a
const x _ = x

flip :: (a -> b -> c) -> (b -> a -> c)
flip f y x = f x y

-- error :: String -> a

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