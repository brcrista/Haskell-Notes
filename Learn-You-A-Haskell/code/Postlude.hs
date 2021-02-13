-- This module contains Prelude-like functions that aren't actually in Prelude.
module Postlude(
  negative,
  cartesian,
  cartesian3,
  cartesianPower
)
where

import Control.Monad

negative :: Integer -> Integer
negative = negate . abs

cartesian :: Applicative f => f a -> f b -> f (a, b)
cartesian xs ys = (,) <$> xs <*> ys

cartesian3 :: Applicative f => f a -> f b -> f c -> f (a, b, c)
cartesian3 xs ys zs = (,,) <$> xs <*> ys <*> zs

cartesianPower :: Applicative f => f a -> f (a, a)
cartesianPower = join cartesian

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x, y, z)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

toList :: Foldable t => t a -> [a]
toList = foldr (:) []