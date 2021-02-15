{-# LANGUAGE NoImplicitPrelude #-}

module Base.Prelude.Num where

import GHC.Num
import GHC.Real
import Base.Prelude.Base (Bool)
import Base.Prelude.Classes (Eq(..))

subtract :: Num a => a -> a -> a
subtract x y = y - x

even :: Integral a => a -> Bool
even n = n `mod` 2 == 0

odd :: Integral a => a -> Bool
odd n = n `mod` 2 /= 0