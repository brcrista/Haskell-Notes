-- This module contains Prelude-like functions that aren't actually in Prelude.
module Postlude(
  cartesian,
  cartesian3,
  cartesianPower,
  curry3,
  first,
  negative,
  toList,
  uncurry3
)
where

import Control.Applicative (liftA2, liftA3)
import Control.Monad (join)

negative :: Integer -> Integer
negative = negate . abs

cartesian :: Applicative f => f a -> f b -> f (a, b)
cartesian = liftA2 (,)

cartesian3 :: Applicative f => f a -> f b -> f c -> f (a, b, c)
cartesian3 = liftA3 (,,)

cartesianPower :: Applicative f => f a -> f (a, a)
cartesianPower = join cartesian

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x, y, z)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

toList :: Foldable t => t a -> [a]
toList = foldr (:) []

-- Using `zipWith` instead of length` lets this work with infinite lists.
indices :: [a] -> [Int]
indices = zipWith const [0 ..]

first :: [Maybe a] -> Maybe a
first ((Just x) : xs) = Just x
first (Nothing  : xs) = first xs
first []              = Nothing

-- Pointfree helpers
range :: Enum a => a -> a -> [a]
range a b = [a .. b]