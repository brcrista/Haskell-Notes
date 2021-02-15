{-# LANGUAGE NoImplicitPrelude #-}

module Base.Prelude.Base(
  Applicative(..),
  Functor(..),
  Monad(..),
  String
) where

import Base.Data.Function (const, (.))
-- TODO minimize GHC imports
import GHC.Types (Char)

type String = [Char]

-- TODO <$>
-- TODO Monoid
-- TODO Semigroup

-- | An instance of `Functor` must obey the following laws:
-- | 1. `fmap . id = id`
-- | 2. `fmap (f . g) = (fmap f) . (fmap g)`
class Functor f where
  fmap  :: (a -> b) -> f a -> f b
  (<$)  :: a -> f b -> f a

  (<$)  = fmap . const

-- TODO Functor instances for tuples and functions

-- TODO Applicative laws
class Functor f => Applicative f where
  pure   :: a -> f a
  (<*)   :: f a -> f b -> f a
  (*>)   :: f a -> f b -> f b
  (<*>)  :: f (a -> b) -> f a -> f b

-- | An instance of `Monad` must obey the following laws:
-- | 1. `return x >>= f = f x`
-- | 2. `m >>= return = m`
-- | 3. `m >>= f >>= g = m >>= (\x -> f x >>= g)`
class Applicative m => Monad m where
  return :: a -> m a
  (>>)   :: m a -> m b -> m b
  (>>=)  :: m a -> (a -> m b) -> m b

  return = pure