module FizzBuzz (
    fizzBuzz
) where

fizzBuzz :: Int -> [String]
fizzBuzz n = fmap fizzBuzzSingle [0 .. n]

a `divides` b = b `mod` a == 0

fizzBuzzSingle :: Int -> String
fizzBuzzSingle n
    | 3 `divides` n && 5 `divides` n = "fizzbuzz"
    | 3 `divides` n = "fizz"
    | 5 `divides` n = "buzz"
    | otherwise = show n