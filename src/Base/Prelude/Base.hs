{-# LANGUAGE NoImplicitPrelude #-}

module Prelude.Base(
  Applicative(..),
  Bool(..),
  Functor(..),
  Monad(..),
  String,
  (.),
  (&&),
  (++),
  (||),
  ($),
  const,
  error,
  flip,
  id,
  map,
  not,
  otherwise,
  undefined
) where

-- TODO minimize GHC imports
import GHC.Base (Bool(..), error, undefined, (++))
import GHC.Classes (Eq, Ord((<), (>)))
import GHC.Show (Show)
import GHC.Types (Char)

-- Trying to use a custom Bool causes some problems.
-- GHC.Num functions return GHC.Base.Bool,
-- and guards depend on a LHS evaluating to GHC.Base.Bool.
-- data Bool = False | True deriving (Eq, Ord, Show)

not :: Bool -> Bool
not False = True
not True  = False

(&&) :: Bool -> Bool -> Bool
True && True = True
_    && _    = False

(||) :: Bool -> Bool -> Bool
False || False = False
_     || _     = True

otherwise :: Bool
otherwise = True

-- This would be the definition of [a] if it were valid Haskell:
-- data [a] = [] | a : [a]

type String = [Char]

id :: a -> a
id x = x

const :: a -> b -> a
const x _ = x

flip :: (a -> b -> c) -> (b -> a -> c)
flip f y x = f x y

($) :: (a -> b) -> a -> b
f $ x = f x

(.) :: (b -> c) -> (a -> b) -> (a -> c)
(f . g) x = f (g x)

map :: (a -> b) -> [a] -> [b]
map f xs = [f x | x <- xs]

-- TODO fixity declarations for operators
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

instance Functor [] where
  fmap = map

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