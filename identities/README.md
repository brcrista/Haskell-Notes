# Identities

```hs
-- From https://wiki.haskell.org/Currying>
(,) = curry id
fst = uncurry const
snd = (uncurry . flip) const
swap = (uncurry . flip . curry) id

[f x y | x <- xs, y <- ys] = f <$> xs <*> ys
[f x y | x <- xs, y <- ys] = xs >>= \x -> ys >>= \y -> return (f x y)

[0 .. n] >>= const xs = take (n * length xs) $ cycle xs
```