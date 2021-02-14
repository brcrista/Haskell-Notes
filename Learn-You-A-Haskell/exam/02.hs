import Prelude()

(.) :: (b -> c) -> (a -> b) -> a -> c
(f . g) x = f (g x)

($) :: (a -> b) -> a -> b
f $ x = f x