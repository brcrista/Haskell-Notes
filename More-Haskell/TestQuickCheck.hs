#!/usr/bin/env stack runghc

-- Prerequisite: stack install QuickCheck

{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck

commutative :: Eq b => (a -> a -> b) -> a -> a -> Bool
commutative f x y = f x y == f y x

involutory :: Eq a => (a -> a) -> a -> Bool
involutory f x = f (f x) == x

prop_reverseIsInvolutory :: [Int] -> Bool
prop_reverseIsInvolutory = involutory reverse

prop_integerAdditionIsCommutative :: Integral a => a -> a -> Bool
prop_integerAdditionIsCommutative = commutative (+)

prop_integerSubtractionIsCommutative :: Integral a => a -> a -> Bool
prop_integerSubtractionIsCommutative = commutative (-)

return []
main = $quickCheckAll
