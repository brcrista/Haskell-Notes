-- This module contains Prelude-like functions that aren't actually in Prelude.
module Postlude(
  (|>),
  negative,
  cartesian
)
where

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

negative :: Integer -> Integer
negative = negate . abs

cartesian :: Applicative f => f a -> f b -> f (a, b)
cartesian xs ys = (,) <$> xs <*> ys

cartesian3 :: Applicative f => f a -> f b -> f c -> f (a, b, c)
cartesian3 xs ys zs = (,,) <$> xs <*> ys <*> zs
