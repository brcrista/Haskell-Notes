-- This module contains Prelude-like functions that aren't actually in Prelude.
module Postlude(
  negative,
  cartesian
)
where

-- W combinator
-- Turn a binary function into a unary function by duplicating its argument.
join :: (a -> a -> b) -> a -> b
join f x = f x x

negative :: Integer -> Integer
negative = negate . abs

cartesian :: Applicative f => f a -> f b -> f (a, b)
cartesian xs ys = (,) <$> xs <*> ys

cartesian3 :: Applicative f => f a -> f b -> f c -> f (a, b, c)
cartesian3 xs ys zs = (,,) <$> xs <*> ys <*> zs

cartesianPower :: Applicative f => f a -> f (a, a)
cartesianPower = join cartesian