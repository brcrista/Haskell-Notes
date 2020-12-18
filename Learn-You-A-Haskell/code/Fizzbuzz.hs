divisible :: Int -> Int -> Bool
divisible a b = a `mod` b == 0

fizzBuzzInner :: Int -> String
fizzBuzzInner n
  | n `divisible` 3 && n `divisible` 5 = "fizzbuzz"
  | n `divisible` 3 = "fizz"
  | n `divisible` 5 = "buzz"
  | otherwise = show n

-- fizzBuzz 20
-- ["1","2","fizz","4","buzz","fizz","7","8","fizz","buzz","11","fizz","13","14","fizzbuzz","16","17","fizz","19","buzz"]
fizzBuzz :: Int -> [String]
fizzBuzz n = map fizzBuzzInner [1 .. n]
