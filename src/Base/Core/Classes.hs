{-# LANGUAGE NoImplicitPrelude #-}

module Base.Core.Classes(
  Eq(..),
  Ord(..),
  compare,
  max,
  min
) where

import Base.Data.Bool (otherwise)
import GHC.Classes (Ord((<), (<=), (>), (>=)), Eq((==), (/=)))
import GHC.Types (Ordering(..))

compare :: Ord a => a -> a -> Ordering
compare x y
  | x < y = LT
  | x > y = GT
  | otherwise = EQ

max :: Ord a => a -> a -> a
max x y
  | x > y = x
  | otherwise = y

min :: Ord a => a -> a -> a
min x y
  | x < y = x
  | otherwise = y