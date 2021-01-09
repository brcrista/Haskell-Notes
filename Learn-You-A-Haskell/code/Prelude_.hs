module Prelude_
  ( all',
    and',
    any',
    -- break',
    compare',
    -- concat',
    -- concatMap',
    const',
    curry',
    cycle',
    drop',
    dropWhile',
    either',
    elem',
    -- error',
    even',
    filter',
    flip',
    foldl',
    foldl1',
    -- foldMap',
    foldr',
    foldr1',
    fst',
    -- gcd',
    getLine',
    head',
    id',
    init',
    -- iterate',
    last',
    length',
    -- lines',
    -- lookup',
    map',
    max',
    maximum',
    min',
    minimum',
    not',
    -- notElem',
    null',
    odd',
    or',
    otherwise',
    print',
    product',
    putStr',
    putStrLn',
    repeat',
    replicate',
    reverse',
    scanl',
    scanl1',
    scanr',
    scanr1',
    snd',
    -- span',
    -- splitAt',
    subtract',
    sum',
    tail',
    take',
    takeWhile',
    uncurry',
    -- unlines',
    -- unwords',
    -- words',
    zip',
    -- zip3',
    zipWith',
    -- zipWith3'
  )
where

-- Basic functions

not' :: Bool -> Bool
not' False = True
not' True = False

id' :: a -> a
id' x = x

const' :: a -> b -> a
const' x _ = x

curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f x y = f (x, y)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f (x, y) = f x y

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f y x = f x y

-- error' :: String -> a

otherwise' :: Bool
otherwise' = True

compare' :: Ord a => a -> a -> Ordering
compare' x y
  | x < y = LT
  | x > y = GT
  | otherwise' = EQ

max' :: Ord a => a -> a -> a
max' x y
  | x > y = x
  | otherwise' = y

min' :: Ord a => a -> a -> a
min' x y
  | x < y = x
  | otherwise' = y

fst' :: (a, b) -> a
fst' (x, _) = x

snd' :: (a, b) -> b
snd' (_, y) = y

zip' :: [a] -> [b] -> [(a, b)]
zip' = zipWith' (,)

-- zip3' :: [a] -> [b] -> [c] -> [(a, b, c)]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys
zipWith' _ _ _ = []

-- zipWith3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]

-- List functions

emptyListError = error "empty list"

head' :: [a] -> a
head' [] = emptyListError
head' (x : _) = x

tail' :: [a] -> [a]
tail' [] = emptyListError
tail' (_ : xs) = xs

init' :: [a] -> [a]
init' [] = emptyListError
init' [x] = []
init' (x : xs) = x : init' xs

last' :: [a] -> a
last' [] = emptyListError
last' [x] = x
last' (x : xs) = last' xs

repeat' :: a -> [a]
repeat' x = x : repeat' x

replicate' :: Int -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise' = x : replicate' (n - 1) x

cycle' :: [a] -> [a]
cycle' [] = emptyListError
cycle' xs = xs ++ cycle' xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

-- iterate' :: (a -> a) -> a -> [a]

take' :: Int -> [a] -> [a]
take' _ [] = []
take' n (x : xs)
  | n < 1 = []
  | otherwise' = x : take' (n - 1) xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x : xs)
  | f x = x : takeWhile' f xs
  | otherwise' = []

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' n all@(x : xs)
  | n < 1 = all
  | otherwise' = drop' (n - 1) xs

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f all@(x : xs)
  | f x = dropWhile' f xs
  | otherwise' = all

-- lookup' :: Eq a => a -> [(a, b)] -> Maybe b

-- break' :: (a -> Bool) -> [a] -> ([a], [a])

-- span' :: (a -> Bool) -> [a] -> ([a], [a])

-- splitAt' :: Int -> [a] -> ([a], [a])

map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = [x | x <- xs, f x]

-- Folds and Scans

-- Replace Foldable with lists until I know more about Foldable
-- foldl' :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ acc [] = acc
foldl' f acc (x : xs) = foldl' f (f acc x) xs

-- foldl1' :: Foldable t => (a -> a -> a) -> t a -> a
foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' f [] = emptyListError
foldl1' f (x : xs) = foldl' f x xs

-- foldr' :: Foldable t => (a -> b -> b) -> b -> t a -> b
-- Test case: these expressions should terminate (they won't with `foldl`)
--   foldr (\x y -> x) 0 [1..]
--   take 10 $ foldr (:) [] [1..]
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ acc [] = acc
foldr' f acc (x : xs) = f x (foldr' f acc xs)

-- foldr1' :: Foldable t => (a -> a -> a) -> t a -> a
foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' f (x : xs) = foldr' f x xs

scanl' :: (b -> a -> b) -> b -> [a] -> [b]
scanl' _ acc [] = [acc]
scanl' f acc (x : xs) = acc : scanl' f (f acc x) xs

scanl1' :: (a -> a -> a) -> [a] -> [a]
scanl1' f [] = emptyListError
scanl1' f (x : xs) = scanl' f x xs

scanr' :: (a -> b -> b) -> b -> [a] -> [b]
scanr' f acc xs = scanl' (flip' f) acc (reverse' xs)

scanr1' :: (a -> a -> a) -> [a] -> [a]
scanr1' f xs = scanl1' (flip' f) (reverse' xs)

-- and' :: Foldable t => t Bool -> Bool
and' :: [Bool] -> Bool
and' = foldl' (&&) True

-- or' :: Foldable t => t Bool -> Bool
or' :: [Bool] -> Bool
or' = foldl' (||) False

-- sum' :: (Num a, Foldable t) => t a -> a
sum' :: Num a => [a] -> a
sum' = foldl' (+) 0

-- product' :: (Num a, Foldable t) => t a -> a
product' :: Num a => [a] -> a
product' = foldl' (*) 1

-- maximum' :: (Ord a, Foldable t) => t a -> a
maximum' :: Ord a => [a] -> a
maximum' = foldl1' max'

-- minimum' :: (Ord a, Foldable t) => t a -> a
minimum' :: Ord a => [a] -> a
minimum' = foldl1' min'

-- any' :: Foldable t => (a -> Bool) -> t a -> Bool
any' :: (a -> Bool) -> [a] -> Bool
any' f = or' . map' f

-- all' :: Foldable t => (a -> Bool) -> t a -> Bool
all' :: (a -> Bool) -> [a] -> Bool
all' f = and' . map f

-- elem' :: (Eq a, Foldable t) => a -> t a -> Bool
elem' :: Eq a => a -> [a] -> Bool
elem' x = any' (== x)

-- notElem' :: (Eq a, Foldable t) => a -> t a -> Bool
notElem' :: Eq a => a -> [a] -> Bool
notElem' x = not' . elem' x

-- length' :: Foldable t => t a -> Int
length' :: [a] -> Int
length' = foldl' plusOne 0
  where
    plusOne acc _ = acc + 1

-- null' :: Foldable t => t a -> Bool
null' :: [a] -> Bool
null' [] = True
null' xs = False

-- concat' :: Foldable t => t [a] -> [a]
-- concatMap' :: Foldable t => (a -> [b]) -> t [a] -> [b]

-- Numeric functions

subtract' :: Num a => a -> a -> a
subtract' x y = y - x

even' :: Integral a => a -> Bool
even' n = n `mod` 2 == 0

odd' :: Integral a => a -> Bool
odd' n = n `mod` 2 /= 0

-- String functions

-- unlines' :: [String] -> String

-- unwords' :: [String] -> String
-- words' :: String -> [String]

-- lines' :: String -> [String]

-- Either / Maybe functions
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' left right (Left l) = left l
either' left right (Right r) = right r

maybe' :: b -> (a -> b) -> Maybe a -> b
maybe' _ f (Just x) = f x
maybe' y _ Nothing = y

-- I/O functions

getLineRecursive' :: String -> IO String
getLineRecursive' xs = do
  nextChar <- getChar
  if nextChar == '\n'
    then return xs
    else getLineRecursive' (xs ++ [nextChar])

getLine' :: IO String
getLine' = getLineRecursive' []

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x : xs) = do
  putChar x
  putStr' xs

putStrLn' :: String -> IO ()
putStrLn' xs = do
  putStr' xs
  putChar '\n'

print' :: Show a => a -> IO ()
print' = putStrLn' . show