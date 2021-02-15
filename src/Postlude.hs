-- This module contains Prelude-like functions that aren't actually in Prelude.
module Postlude where

import Control.Applicative (liftA2, liftA3)
import Control.Monad (join)

negative :: Num a => a -> a
negative = negate . abs

-- | The Cartesian product `S1 x S2`.
cartesian :: Applicative f => f a -> f b -> f (a, b)
cartesian = liftA2 (,)

-- | The Cartesian product `S1 x S2 x S3`.
cartesian3 :: Applicative f => f a -> f b -> f c -> f (a, b, c)
cartesian3 = liftA3 (,,)

-- | The Cartesian product of a set with itself, e.g. `R^2`, 
cartesianSquare :: Applicative f => f a -> f (a, a)
cartesianSquare = join cartesian

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x, y, z)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

toList :: Foldable t => t a -> [a]
toList = foldr (:) []

-- | The range of indices for a list.
-- Implementation note: Using `zipWith` instead of length` lets this work with infinite lists.
indices :: [a] -> [Int]
indices = zipWith const [0 ..]

-- | The first `Just` element in a list or `Nothing` if there is none.
first :: [Maybe a] -> Maybe a
first ((Just x) : xs) = Just x
first (Nothing  : xs) = first xs
first []              = Nothing

-- Pointfree helpers
range :: Enum a => a -> a -> [a]
range a b = [a .. b]