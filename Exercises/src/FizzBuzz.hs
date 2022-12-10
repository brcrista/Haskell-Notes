import System.Environment

main = do
    args <- getArgs
    let count = read $ head args
    putStrLn $ unwords $ fizzBuzz count

fizzBuzz :: Int -> [String]
fizzBuzz n = fmap fizzBuzzSingle [0 .. n]

a `divides` b = b `mod` a == 0

fizzBuzzSingle :: Int -> String
fizzBuzzSingle n
    | 3 `divides` n && 5 `divides` n = "fizzbuzz"
    | 3 `divides` n = "fizz"
    | 5 `divides` n = "buzz"
    | otherwise = show n