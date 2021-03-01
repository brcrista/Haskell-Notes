# Identities

```hs
-- From https://wiki.haskell.org/Currying
(,) = curry id
fst = uncurry const
snd = (uncurry . flip) const
swap = (uncurry . flip . curry) id

-- From https://crypto.stanford.edu/~blynn/haskell/why.html
const <*> const = id
const (<*>) <*> const = (.)

-- Functor
fmap f $ xs = f <$> xs

-- Applicative
[f x y | x <- xs, y <- ys] = f <$> xs <*> ys

-- Monad
[f x y | x <- xs, y <- ys] = xs >>= \x -> ys >>= \y -> return (f x y)

m >>= f = join $ fmap f m

-- import Control.Monad
filter pred xs = xs >>= \x -> guard (pred x) >> return x

[0 .. n] >>= const xs = take (n * length xs) $ cycle xs

-- import Data.Function
-- From http://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Function.html#v:on
f `on` id = f
(f `on` g) `on` h = f `on` (g . h)
flip on f . flip on g = flip on (g . f)
```