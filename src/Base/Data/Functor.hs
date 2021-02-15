{-# LANGUAGE NoImplicitPrelude #-}

module Base.Data.Functor where

import Base.Data.Function (const, (.))

-- | An instance of `Functor` must obey the following laws:
-- | 1. `fmap . id = id`
-- | 2. `fmap (f . g) = (fmap f) . (fmap g)`
class Functor f where
  fmap  :: (a -> b) -> f a -> f b
  (<$)  :: a -> f b -> f a

  (<$)  = fmap . const

-- TODO <$>
-- TODO Functor instances for tuples and functions