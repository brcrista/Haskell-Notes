# Foldable and Traversable

We have already seen that lists are foldable and traversable:
functions such as `foldl` and `sequence` are defined on lists.
Now, we will generalize that concept with the `Foldable` and `Traversable` typeclasses.

## Foldable

Any instance of `Foldable` must also be a `Monoid`, meaning it defines `mempty` and `mappend`.
Not surprisingly, `foldl`, `foldl1`, `foldr`, `foldr1`, and `foldMap` all belong to `Foldable`.
However, many functions that are defined in terms of folds are implemented for foldables as well: `elem`, `length`, `null`, and `sum` to name a few.
`concat` is also part of the `Foldable` definition, which illustrates why a `Foldable` must also be a `Monoid`.

We've seen that lists are foldable.
`Maybe`, `Either`, and `(,)` are also foldable.
Note that folds always return an unwrapped version of their data.
Compare `foldl` and `fmap`:

```hs
> fmap (+1) $ Just 2
Just 3

> foldl (+) 1 $ Just 2
3

> fmap (+1) Nothing
Nothing

> foldl (+) 1 Nothing
1

> sum $ Just 1
1

> fmap (+1) $ Left 2
Left 2

> sum $ Left 2
0

> fmap (+1) $ Right 2
Right 3

> sum $ Right 2
2
```

For `Either`, `foldl` applies its function only to `Right` value to fit the pattern where a `Right` value represents a result and a `Left` value represents an error.

## Traversable

A `Traversable` type is both a `Foldable` and a `Functor`.
This makes it a bit like `MonadPlus` since it is both a `Functor` and a `Monoid`.
However, not every `MonadPlus` need be `Traversable`.
For example, `IO` implements `MonadPlus` but not `Traversable` (at least in `Prelude`).

`Traversable`'s premier function is `traverse`.
It's kind of like the opposite of `fmap`:

```hs
> traverse (:[]) $ Just 1
[Just 1]

> fmap Just [1]
[Just 1]

> traverse Just [1 .. 3]
Just [1,2,3]

> fmap Just [1 .. 3]
[Just 1, Just 2, Just 3]

> fmap (:[]) $ Just 1
Just [1]
```

`Traversable` also defines `sequence`, which turns a traversable of monads into a monad of traversables:

```hs
> :t sequence
sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)

> sequence [[1 .. 3]]
[[1], [2], [3]]

> sequence [[1 .. 3], []]
[]

> sequence [Just 1, Just 2, Just 3]
Just [1, 2, 3]

> sequence [Just 1, Nothing, Just 3]
Nothing

> sequence $ Just [1 .. 3]
[Just 1, Just 2, Just 3]

> sequence Nothing
Nothing
```

In `Prelude`, the same types that are `Foldable` are `Traversable`: lists, `Maybe`, `Either`, and `(,)`.