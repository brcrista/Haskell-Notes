-- This module contains Prelude-like functions that aren't actually in Prelude.
module Postlude(
  (|>),
  negative
)
where

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

negative :: Integer -> Integer
negative = negate . abs
