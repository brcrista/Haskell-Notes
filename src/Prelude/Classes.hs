{-# LANGUAGE NoImplicitPrelude #-}

module Prelude.Classes(
  Eq(..),
  Ord(..),
  compare,
  max,
  min
) where

import GHC.Classes (Ord((<), (<=), (>), (>=)), Eq((==), (/=)))
import GHC.Types (Ordering(..))
import Prelude.Base (otherwise)

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