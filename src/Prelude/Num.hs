{-# LANGUAGE NoImplicitPrelude #-}

module Prelude.Num where

import Prelude.Base (Bool)

subtract :: Num a => a -> a -> a
subtract x y = y - x

even :: Integral a => a -> Bool
even n = n `mod` 2 == 0

odd :: Integral a => a -> Bool
odd n = n `mod` 2 /= 0