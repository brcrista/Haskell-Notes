import Prelude()

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

repeat :: a -> [a]
repeat x = x : repeat x

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ acc []       = acc
foldl f acc (x : xs) = foldl f (f acc x) xs