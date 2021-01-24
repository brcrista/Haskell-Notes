{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck

commutative :: Eq b => (a -> a -> b) -> a -> a -> Bool
commutative f x y = f x y == f y x

involutive :: Eq a => (a -> a) -> a -> Bool
involutive f x = f (f x) == x

prop_reverseIsInvolutive :: [Int] -> Bool
prop_reverseIsInvolutive = involutive reverse

-- prop_integerAdditionIsCommutative :: Integral a => a -> a -> Bool
prop_integerAdditionIsCommutative :: Int -> Int -> Bool
prop_integerAdditionIsCommutative = commutative (+)

-- -- prop_integerSubtractionIsCommutative :: Integral a => a -> a -> Bool
prop_integerSubtractionIsCommutative :: Int -> Int -> Bool
prop_integerSubtractionIsCommutative = commutative (-)

return []
main = $quickCheckAll