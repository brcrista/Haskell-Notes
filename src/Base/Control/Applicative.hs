{-# LANGUAGE NoImplicitPrelude #-}

module Base.Control.Applicative where

import Base.Data.Functor

-- TODO Applicative laws
class Functor f => Applicative f where
  pure   :: a -> f a
  (<*)   :: f a -> f b -> f a
  (*>)   :: f a -> f b -> f b
  (<*>)  :: f (a -> b) -> f a -> f b