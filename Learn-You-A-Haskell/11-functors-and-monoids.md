# Functors, Applicative Functors, and Monoids

## More on functors

We're already seen a few types that are instances of `Functor`:

```hs
[]
Either a
Maybe
```

Recall that a functor implements a function `fmap`:

```hs
fmap :: Functor f => (a -> b) -> f a -> f b
```

So for `[]`, we have `fmap = map`, where

```hs
> :t map
(a -> b) -> [a] - [b]
```

`IO` and `(->) r` are also functors.
The behavior of `fmap` for `IO` is pretty obvious.
`(->) r` means a function with type `r -> a` is a functor for any `a`.
So, with `f = (-> r)` in the `fmap` type constraint, we have:

```hs
fmap :: (a -> b) -> (r -> a) -> (r -> b)
```

This looks a lot like the signature of function composition:

```hs
(.) :: (b -> c) -> (a -> b) -> (a -> c)
```

So, mapping a function over another function is the same as calling that function on the first function's result, as you'd expect.