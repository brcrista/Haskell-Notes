# `Identity`

The `Data.Functor.Identity` module contains the type constructor `Identity a`.
It is defined as

```hs
newtype Identity a = Identity { runIdentity :: a }
```

If it wasn't for the selector function, it could just be written as

```hs
data Identity a = Identity a
```

which shows what it is a little more clearly.
It's basically the `id` function but on types.

## Typeclass implementations

`Identity` is a member of basically every typeclass.
In fact, it often works well as a minimal example of what a typeclass implementation should be.
The [real-life definiiton](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.15.0.0/src/Data-Functor-Identity.html#Identity) uses `Data.Coerce.coerce` for most functions, but we'll use `Id` to illustrate.

The functor implementation is:

```hs
instance Functor Identity where
    fmap f (Identity x) = Identity (f x)
```

The monad implementation is:

```hs
instance Monad Identity where
    return x = Identity x
    ix >>= f = f $ runIdentity ix
```