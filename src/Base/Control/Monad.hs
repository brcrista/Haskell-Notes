{-# LANGUAGE NoImplicitPrelude #-}

module Base.Control.Monad where

import Base.Control.Applicative

-- | An instance of `Monad` must obey the following laws:
-- | 1. `return x >>= f = f x`
-- | 2. `m >>= return = m`
-- | 3. `m >>= f >>= g = m >>= (\x -> f x >>= g)`
class Applicative m => Monad m where
  return :: a -> m a
  (>>)   :: m a -> m b -> m b
  (>>=)  :: m a -> (a -> m b) -> m b

  return = pure