# Combinators

A **combinator** is a function that is defined only in terms of function application and other combinators.
In fact, we've already met several combinators: `.`, `$`, `id`, `const`, `flip`, `fmap`, `<*>`, and `=<<`.
However, a function like `succ n = n + 1` is not a combinator because it uses `+` and `1`.
So a more precise way to define a combinator is as a function that only uses names that are passed in as parameters.

Combinators are useful for creating new functions out of old ones in well-known ways.
They are also useful for defining functions in **point-free** style.
The mathematical study of combinators is related to lambda calculus and is one of the things that Haskell Curry is known for.

Common combinators all have one-letter mathematical names that aren't especially important for our purposes, but you can see the list [here](https://gist.github.com/Avaq/1f0636ec5c8d6aed2e45).

## `Data.Function`

The [`Data.Function`](http://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Function.html) module is pretty slim, but it contains definitions for a few useful combinators.
Fortunately, we've already met most of them: `.`, `$`, `id`, `const`, and `flip`.

The `&` operator is just the flip of `$`:

```hs
(&) :: a -> (a -> b) -> b
x & f = f x
```

Naturally, it has the algebraic property that `flip ($) = (&)`.
Its main use is for chaining a series of functions with postfix syntax, like the `|` operator on the command line or the `|>` operator in F#.

The `on` function has the signature:

```hs
on :: (b -> b -> c) -> (a -> b) -> (a -> a -> c)
```

So, it lets you map a binary function's parameters to a different type.
In that sense it's analogous to function composition for unary functions.
This is called the **P combinator**.

For example, consider

```hs
> compare (1, 5) (2, 1)
LT
```

The implementation of `Ord` for tuples will always compare the first elements.
We can make it compare the second element with:

```hs
> (compare `on` snd) (1, 5) (2, 1)
GT
```

The last function exported by `Data.Function` is `fix`, which we'll come back to at the end.

## `join` as a combinator

We've seen `Control.Monad.join` as a function that "flattens" a monad:

```hs
> :t join
join :: Monad m => m (m a) -> m a

> join $ Just $ Just 1
Just 1
```

But remember: functions are also monads!
Substituting `r ->` for `m`, its type is

```hs
join :: (r -> r -> a) -> r -> a
```

or, equivalently

```hs
join :: (a -> a -> b) -> a -> b
```

So, if both of a binary function's arguments are of the same type, you can de-dupe it to get a unary function that just passes the same thing twice.
This is called the **W combinator**.

For example, the reflexive property of `==` is

```hs
x == x = True
```

You could write this point-free as

```hs
join (==) = const True
```

You could also do

```hs
> square = join (*)
> square 5
25
```

## Functions as applicative functors

Of course, functions are also applicative functors.
Here's the type signature for `<*>`:

```hs
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
```

If we substitute `Applicative (r ->)` here, we get

```hs
(<*>) :: (r -> a -> b) -> (r -> a) -> r -> b
```

Renaming the variables:

```hs
(<*>) :: (a -> b -> c) -> (b -> c) -> a -> c
```

This looks a lot like a combinator!
This is known as the **S combinator**.
For functions, `<*>` is [implemented as](https://hackage.haskell.org/package/base-4.14.0.0/docs/src/GHC.Base.html#line-973)

```hs
f <*> g x = f x (g x)
```

For functions, `<*>` substitutes the 1st parameter for the 2nd by applying a function to it.
In that sense, it works a lot like composition (`<$>`) and `on`.
It also works like `join` in that it turns a binary function into a `unary` function.

## The `fix` function

- <https://mvanier.livejournal.com/2897.html>
- <http://www.vex.net/~trebla/haskell/fix.xhtml>
- <https://medium.com/@cdsmithus/fixpoints-in-haskell-294096a9fc10>
- <https://en.wikibooks.org/wiki/Haskell/Fix_and_recursion>

`Data.Function.fix` returns the least **fixed point** of a function. It's defined as:

```hs
fix :: (a -> a) -> a
fix f = x where x = f x
```

It's defined exactly as it sounds, but how does this work? It also seems to get caught in infinite loops easily, as you'd expect:

```hs
> fix id
-- Loops forever
> let square = join (*)
> fix square
-- Loops forever
> fix (const 10)
10
```

However, its purpose isn't really for finding fixed points, but for encoding recursion as a function.

### Fixed points in Haskell

We can define a function that looks for a fixed point from a starting guess:

```hs
fixpoint :: (a -> a) -> a -> a
fixpoint f x
  | f x == x = x
  | otherwise = fixpoint f (f x)
```

This will find the fixpoint when `f` *converges* to its fixed point from a starting point:

```hs
> fixpoint id 1
1
> fixpoint id 10
> fixpoint square 1
1
> fixpoint square 0
0
> fixpoint square 2
-- Loops forever
> fixpoint square (-1)
1
> fixpoint (const 10) 1
10
```

However, it's obvious that not every function as a fixed point, such as `succ`.

Haskell semantically rescues its fixed function (and its notions of functions in general) by adding the **bottom** value (⊥) to every set. (This is in the realm of denotational semantics, which means how Haskell programs map to theoretical mathematical concepts.) ⊥ means that a function never returns a value, either because it loops forever or throws an exception. So then, `fix succ = ⊥` since `succ ⊥ = ⊥`.

### Y combinator

`fix` is an implementation of the **Y combinator**.
We can think of this as a function that defines recursion generally for all other functions.

Consider the definition of a factorial:

```hs
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

This uses **explicit recursion** because `factorial` is defined in terms of itself.
We can remove this though by passing in the function to call as a parameter:

```hs
almostFactorial _ 0 = 1
almostFactorial f n = n * f (n - 1)
```

Now, there's no recursion here (at least, not *explicit recursion*).
But some function `f` exists that turns `almostFactorial` into `factorial`.
The Y combinator finds this function:

```hs
import Data.Function (fix)

factorial = fix almostFactorial
```