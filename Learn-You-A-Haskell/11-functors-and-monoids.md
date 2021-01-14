# Functors, Applicative Functors, and Monoids

## Functions as Functors

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

So, mapping a function over another function is the same as calling that function on the first function's result, as you'd expect:

```hs
fmap = (.)
```

## Another view of functors

A first look at functors usually characterize them as a "container" for a value that can be mapped over.
A better fit at this point would be "something that produces values."
- A list produces values when you iterate over it.
- A `Maybe` produces 0 or 1 values.
- A function produces a value when you call it with an argument.

When you map over a functor, you add something to it that changes the values as they come out.

It may seem strange that functions and lists could be fundamentally connected like this.
Consider though the concept of **coroutines**.
Languages like C#, JavaScript, and Python have a concept of "generators."
Basically, these are plain-old coroutines: routines that can be started and stopped and yield values along the way.
The language presents these coroutines as iterators that produce a sequence of values, one call to the coroutine for each element.
Haskell has no iterators; it only has lists, which function much like iterators in other languages.

## Functor Laws

While it's not enforced by Haskell's type system, we define functors mathematically by a set of properties that must hold for the way `fmap` is defined.
This is just like how we observe the reflexive, associative, and transitive properties for any definition of an equality function.
These properties are called the **functor laws.**

1. Functor identity law: `fmap id = id`
1. Distributive property over composition: `fmap (f . g) = fmap f . fmap g`

Basically, these laws ensure that `fmap` applies its function to the functor in a sane way.
Let's play around with some functors to see how these work.

### Lists

```hs
fmap = map
map f xs = [f x | x <- xs]
```

```hs
-- Identity law
fmap id [] = []
fmap id [1, 2] = [id 1, id 2] = [1, 2]

-- Composition law
square x = x * x

fmap ((+1) . square) [1, 2]
= [((+1) . square) 1, ((+1) . square) 2]
= [2, 5]

fmap (+1) . fmap square $ [1, 2]
= fmap (+1) $ fmap square [1, 2]
= fmap (+1) [square 1, square 2]
= [((+1) . square) 1, ((+1) . square) 2]
= [2, 5]
```

### Maybes

```hs
fmap _ Nothing = Nothing
fmap f (Just x) = Just $ f x
```

```hs
-- Identity law
fmap id Nothing = Nothing
fmap id (Just 1) = Just $ id 1 = Just 1

-- Composition law
square x = x * x

fmap ((+1) . square) (Just 1)
= Just $ ((+1) . square) 1
= Just 2

fmap (+1) . fmap square $ Just 1
= fmap (+1) $ Just $ square 1
= Just $ (+1) $ square 1
= Just $ ((+1) . square) 1
= Just 2
```

### Functions

```hs
fmap = (.)
```

```hs
-- Identity law
fmap id id = id . id = id
fmap id (+1) = id . (+1) = (+1)

-- Composition law
square x = x * x

fmap ((+1) . square) (+1)
= (+1) . square . (+1)

fmap (+1) . fmap square $ (+1)
= fmap (+1) $ square . (+1)
= (+1) . square . (+1)
```

### Pathological functor

Let's come up with an implementation of `fmap` that breaks the laws.

```hs
fmap :: (a -> b) -> [a] -> [b]
fmap _ _ = []
```

```hs
-- Identity law
fmap id [] = []
fmap id [1, 2] = [] -- Nope!

-- Composition law
square x = x * x

fmap ((+1) . square) [1, 2]
= []

fmap (+1) . fmap square $ [1, 2]
= fmap (+1) $ fmap square [1, 2]
= fmap (+1) []
= [] -- Ok
```

## Applicative functors

What happens when we have a functor of partially applied functions?

```hs
> xs = fmap (*) [1..4]
> :t xs
xs :: (Num a, Enum a) => [a -> a]

> fmap ($ 1) xs
[1,2,3,4]

> fmap ($ 2) xs
[2,4,6,8]
```

So far so good.
But what if we have:

```hs
xs = fmap (*) [0..2]
ys = [1..4]
```

How can we apply each of the `xs` to each of the `ys`?
We would have to write a list comprehension like

```hs
applyList :: [a -> b] -> [a] -> [b]
applyList xs ys = [x y | x <- xs, y <- ys]
```

And what if we wanted to apply a function in a `Maybe` to another `Maybe`?

```hs
x = Just (* 3)
y = Just 2
```

We'd have to write a function like

```hs
applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
applyMaybe Nothing _ = Nothing
applyMaybe (Just f) something = fmap f something
```

The `Applicative` typeclass represents **applicative functors**.
The typeclass is defined with two functions:

```hs
pure :: Applicative f => a -> f a
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
```

The `pure` function takes any value and turns it into an `Applicative`:

```hs
> pure 1 :: [Int]
[1]

> pure 1 :: Maybe Int
Just 1
```

Coming from object-oriented languages, `pure` is interesting.
In Java and C#, interfaces don't usually specify a way to construct an object.

The `<*>` operator is a generalized version of `applyList` and `applyMaybe` up above.
Note that any instance of `Applicative` must also be a `Functor`.

Note the similarity of `<*>` and `fmap`:

```hs
> :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b

> :t (<*>)
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
```

You can even write stuff like

```hs
> Just (+) <*> Just 3 <*> Just 5
Just 8
```

Here, we've "lifted" the `+` operator to work on `Num a => Maybe a`.
We can simplify this using the `<$>` operator, which is just `fmap` as an infix operator.

```hs
> (+) <$> Just 3 <*> Just 5
Just 8
```

This is kind of the zen of applicative functors.
`fmap` gives us a way to apply a unary function to a functor,
and `<*>` gives us a way to apply a binary function to two functors.

### Applicative functor laws

This simplification is guaranteed to work for all applicatives because of the **applicative laws** (namely, the first one).
These laws are:

1. Functor reduction law: `pure f <*> x = f <$> x`
1. Identity law: `pure id <*> x = x`
1. Homomorphism law: `pure f <*> pure x = pure (f x)`
1. Composition law: `pure (.) <*> x <*> y <*> z = x <*> (y <*> z)`
1. Interchange law: `x <*> pure y = pure ($ y) <*> x`

## `Control.Applicative`

The `Control.Applicative` module contains more types and functions related to applicatives.

### `liftA2`

We just saw how we can "lift" a function to work on an applicative functor:

```hs
> (+) <$> Just 3 <*> Just 5
Just 8
```

This can also be achieved with the `liftA2` function:

```hs
> liftedAdd = liftA2 (+)
> Just 3 `liftedAdd` Just 5
Just 8
```

The `2` in `liftA2` refers to the arity of the function.
There's also `liftA` (which is the same as `fmap` except with a more restrictive type constraint) and `liftA3`.

### `ZipList`

We've seen that lists are applicative functors.
However, there are actually *more* ways that they can fulfill the definition.

An alternative way is given by `ZipList`.
`ZipList` wraps a normal list.
Rather than implementing `<*>` through Cartesian product, it does so through `zipWith`.

## Other functions for applicatives

### `sequenceA`

The `sequenceA` function gives us a way to pull an applicative out of square brackets.

```hs
> :t sequenceA
sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
```

For example:

```hs
> sequenceA [Just 1, Just 2]
Just [1, 2]

> sequenceA [Just 1, Just 2, Nothing]
Nothing
```