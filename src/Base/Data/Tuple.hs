{-# LANGUAGE NoImplicitPrelude #-}

module Base.Data.Tuple where

-- This would be the definition of () if it were valid Haskell:
-- data () = ()

fst :: (a, b) -> a
fst (x, _) = x

snd :: (a, b) -> b
snd (_, y) = y

curry :: ((a, b) -> c) -> (a -> b -> c)
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f (x, y) = f x y