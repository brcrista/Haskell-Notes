# Trivial Functors

There are a few functors that aren't useful for general computation but can fill in when building higher-level abstractions.

## Identity

The `Identity` functor is simply a pass-through context that holds a value and maps functions on the value unchanged. It's like a list that has exactly one element.

```hs
> import Data.Functor.Identity
> fmap succ (Identity 0)
Identity 1
```

It fulfills the functor laws:

```hs
-- Identity
fmap id (Identity x)
= Identity (id x)
= Identity x
= id (Identity x)

-- Distributive property over composition
fmap (f . g) (Identity x)
= Identity (f . g $ x)
= Identity (f (g x))
= fmap f (Identity (g x))
= fmap f $ fmap g (Identity x)
= (fmap f . fmap g) (Identity x)
```

## Const

The `Const` functor is the dual of the `Identity` functor: it always preserves its value.

```hs
> import Data.Functor.Const
> fmap succ (Const 0) :: Const Int Int
Const 0
```

It fulfills the functor laws:

```hs
-- Identity
fmap id (Const x)
= Const x
= id (Const x)

-- Distributive property over composition
fmap (f . g) (Const x)
= Const x
= (fmap f . fmap g) (Const x)
```

`Const` has the interesting property of turning `<*>` into `<>` when the inner type is a `Semigroup`:

```hs
> Const "hello" <> Const " world"
Const "hello world"
> Const "hello" <*> Const " world"
Const "hello world"
```

This is because of the type signature of `<*>`:

```hs
-- The type signature of <*> is
(<*>) :: Applicative f => f (a -> b) -> f a -> f b

-- With f = Semigroup s => Const s
(<*>) :: Semigroup s => Const s (a -> b) -> Const s a -> Const s b

-- Since Const ignores its second argument
(<*>) :: Semigroup s => Const s -> Const s -> Const s

-- Compare the type signature for <>
(<>) :: Semigroup s => s -> s -> s

-- So we have
instance Semigroup s => Semigroup (Const s b)
```

## Compose

`Compose` is a functor that lets you map a function over a nested context. It's so named because it composes two functor types -- see [Types and Kinds](#types-and-kinds) below.

```hs
> import Data.Functor.Compose
> fmap succ $ [Just 0]
-- Type error
> fmap succ $ Compose [Just 0]
Compose [Just 1]
```

It fulfills the functor laws :
```hs
-- Identity
fmap id (Compose (f' $ g' x))
= Compose (fmap id $ (f' $ g' x))
= Compose (f' $ fmap id $ g' x)
= Compose (f' $ g' $ id x)
= Compose (f' $ g' $ x)
= id (Compose (f' $ g' x))

-- Distributive property over composition
fmap (f . g) (Compose (f' $ g' x))
= Compose (fmap (f . g) $ (f' $ g' x))
= Compose (fmap f . fmap g $ (f' $ g' x))
= (fmap f . fmap g) (Compose x)
```

## Types and Kinds

Note the kinds of `Identity`, `Const`, and `compose` mirror the types of `id`, `const`, and `.`:

```hs
> :t id
id :: a -> a
> :k Identity
Identity :: * -> *
> :t const
const :: a -> b -> a
> :k Const
Const :: * -> k -> *
> :t (.)
(.) :: (b -> c) -> (a -> b) -> a -> c
> :k Compose
Compose :: (k -> *) -> (k1 -> k) -> k1 -> *
```