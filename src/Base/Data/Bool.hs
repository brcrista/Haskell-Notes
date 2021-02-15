{-# LANGUAGE NoImplicitPrelude #-}

module Base.Data.Bool(
  Bool(..),
  (&&),
  (||),
  bool,
  not,
  otherwise
) where

-- Trying to use a custom Bool causes some problems.
-- GHC.Num functions return GHC.Base.Bool,
-- and guards depend on a LHS evaluating to GHC.Base.Bool.
-- data Bool = False | True deriving (Eq, Ord, Show)
import GHC.Base (Bool(..))

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

bool :: a -> a -> Bool -> a
bool x _ False = x
bool _ y True  = y