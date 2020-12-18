-- This module contains Prelude-like functions that aren't actually in Prelude.
module Postlude(
    apply,
    compose,
    negative
) where

apply :: (a -> b) -> a -> b
apply f = f

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g x = f $ g x

negative = compose negate abs