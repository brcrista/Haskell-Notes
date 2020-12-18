module Prelude_
  ( -- abs',
    all',
    -- and',
    any',
    -- break',
    -- ceiling',
    compare',
    -- concat',
    -- concatMap',
    const',
    -- cos',
    curry',
    cycle',
    -- div',
    drop',
    dropWhile',
    elem',
    -- error',
    even',
    filter',
    flip',
    -- floor',
    foldl',
    foldl1',
    -- foldMap',
    foldr',
    foldr1',
    -- fromIntegral',
    fst',
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
    -- maxBound',
    maximum',
    min',
    -- minBound',
    minimum',
    -- mod',
    -- negate',
    not',
    -- notElem',
    null',
    odd',
    -- or',
    otherwise',
    -- pred',
    product',
    -- read',
    repeat',
    replicate',
    reverse',
    scanl',
    scanl1',
    scanr',
    scanr1',
    -- show',
    -- sin',
    snd',
    -- span',
    -- splitAt',
    -- sqrt',
    subtract',
    -- succ',
    sum',
    tail',
    take',
    takeWhile',
    -- tan',
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

emptyListError = error "empty list"

-- abs' :: Num a => a -> a

-- all' :: Foldable t => (a -> Bool) -> t a -> Bool
all' :: (a -> Bool) -> [a] -> Bool
all' f = foldl' (\acc x -> acc && f x) True

-- and' :: Foldable t => t Bool -> Bool

-- any' :: Foldable t => (a -> Bool) -> t a -> Bool
any' :: (a -> Bool) -> [a] -> Bool
any' f = foldl' (\acc x -> acc || f x) False

-- break' :: (a -> Bool) -> [a] -> ([a], [a])

-- ceiling' :: (RealFrac a, Integral b) => a -> b

compare' :: Ord a => a -> a -> Ordering
compare' x y
  | x < y = LT
  | x > y = GT
  | otherwise' = EQ

-- concat' :: Foldable t => t [a] -> [a]
-- concatMap' :: Foldable t => (a -> [b]) -> t [a] -> [b]

const' :: a -> b -> a
const' x _ = x

-- cos' :: Floating a => a -> a

curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f x y = f (x, y)

cycle' :: [a] -> [a]
cycle' [] = emptyListError
cycle' xs = xs ++ cycle' xs

-- div' :: Integral a => a -> a -> a

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

-- elem' :: (Eq a, Foldable t) => a -> t a -> Bool
elem' :: Eq a => a -> [a] -> Bool
elem' x = any' (== x)

-- error' :: String -> a

even' :: Integral a => a -> Bool
even' n = n `mod` 2 == 0

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = [x | x <- xs, f x]

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f y x = f x y

-- floor' :: (RealFrac a, Integral b) => a -> b

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
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f acc xs = foldl' (flip' f) acc (reverse' xs)

-- foldr1' :: Foldable t => (a -> a -> a) -> t a -> a
foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' f xs = foldl1' (flip' f) (reverse' xs)

-- fromIntegral' :: (Integral a, Num b) => a -> b

fst' :: (a, b) -> a
fst' (x, _) = x

head' :: [a] -> a
head' [] = emptyListError
head' (x : _) = x

id' :: a -> a
id' x = x

init' :: [a] -> [a]
init' [] = emptyListError
init' [x] = []
init' (x : xs) = x : init' xs

-- iterate' :: (a -> a) -> a -> [a]

last' :: [a] -> a
last' [] = emptyListError
last' [x] = x
last' (x : xs) = last' xs

-- length' :: Foldable t => t a -> Int
length' :: [a] -> Int
length' = foldl' plusOne 0
  where
    plusOne acc _ = acc + 1

-- lines' :: String -> [String]

-- lookup' :: Eq a => a -> [(a, b)] -> Maybe b

map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

max' :: Ord a => a -> a -> a
max' x y
  | x > y = x
  | otherwise' = y

-- maxBound' :: Bounded a => a -> a

-- maximum' :: (Ord a, Foldable t) => t a -> a
maximum' :: Ord a => [a] -> a
maximum' = foldl1' max'

min' :: Ord a => a -> a -> a
min' x y
  | x < y = x
  | otherwise' = y

-- minBound' :: Bounded a => a -> a

-- minimum' :: (Ord a, Foldable t) => t a -> a
minimum' :: Ord a => [a] -> a
minimum' = foldl1' min'

-- mod' :: Integral a => a -> a -> a
-- negate' :: Num a => a -> a

not' :: Bool -> Bool
not' False = True
not' True = False

-- notElem' :: (Eq a, Foldable t) => a -> t a -> Bool

-- null' :: Foldable t => t a -> Bool
null' :: [a] -> Bool
null' [] = True
null' xs = False

odd' :: Integral a => a -> Bool
odd' n = n `mod` 2 /= 0

-- or' :: Foldable t => t Bool -> Bool

otherwise' :: Bool
otherwise' = True

-- pred' :: Enum a => a -> a

-- product' :: (Num a, Foldable t) => t a -> a
product' :: Num a => [a] -> a
product' = foldl' (*) 1

-- read' :: Read a => String -> a

repeat' :: a -> [a]
repeat' x = x : repeat' x

replicate' :: Int -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise' = x : replicate' (n - 1) x

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

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

-- show' :: Show a => a -> String
-- sin' :: Floating a => a -> a

snd' :: (a, b) -> b
snd' (_, y) = y

-- span' :: (a -> Bool) -> [a] -> ([a], [a])

-- splitAt' :: Int -> [a] -> ([a], [a])

-- sqrt' :: Floating a => a -> a

subtract' :: Num a => a -> a -> a
subtract' x y = y - x

-- succ' :: Enum a => a -> a

-- sum' :: (Num a, Foldable t) => t a -> a
sum' :: Num a => [a] -> a
sum' = foldl' (+) 0

tail' :: [a] -> [a]
tail' [] = emptyListError
tail' (_ : xs) = xs

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

-- tan' :: Floating a => a -> a

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f (x, y) = f x y

-- unlines' :: [String] -> String

-- unwords' :: [String] -> String
-- words' :: String -> [String]

zip' :: [a] -> [b] -> [(a, b)]
zip' = zipWith' (,)

-- zip3' :: [a] -> [b] -> [c] -> [(a, b, c)]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys
zipWith' _ _ _ = []

-- zipWith3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
