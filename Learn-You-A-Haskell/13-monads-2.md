# Monads

## `Control.Monad.Writer`

The `Control.Monad.Writer` module comes from the `mtl` package, which is included with GHC by default.
You can see all of the packages shipped with GHC with the `ghc-pkg list` command.

The `Writer` type is defined as follows:

```hs
newtype Writer w a = Writer { runWriter :: (a, w) }

instance Monoid w => Monad (Writer w) where
    return x = Writer (x, mempty)
    (Writer (x, v)) >>= f = let Writer (y, v') = f x in Writer (y, v `mappend` v')
```

So, a `Writer` just wraps a pair of a value and some monoid.
Usually, this monoid is going to be a string or a list.
What's happening here is that `f` produces another instance of `Writer`.
In a chain of `>>=` with writers, the final value will be whatever the last function returned and the result of `mappend`-ing all the monoids (e.g. concatenating all of the strings).

`Writer` is good for adding tracing to some code.
You can use `do` syntax and the `tell` function to insert log lines, like

```hs
addOne :: Int -> Writer String Int
addOne n = do
    tell $ show n ++ " + 1"
    return (n + 1)
```

Note that this will change the return type of any function you add logging to.
At the end, use `runWriter` to extract the tuple.

## Difference lists

Haskell lists are linked lists, and appending to the end of a linked list is an `O(n)` operation.
Therefore, the time it takes to concatenated several lists may depend on the order in which they are concatenated.
Ideally, you want to always append to the shorter list.

**Difference lists** provide a way to do this.
For any list `xs`, the difference list is just `(xs ++)`.
This keeps the concatenation right-associative so the left-hand side doesn't grow before appending to it.

## Functions as monads

We've already seen that functions are both functors and applicative functors.
They're also monads:

```hs
instance Monad ((->) r) where
    return  = const
    f >>= g = \x -> g (f x) x
```

The function taken on the right-hand-side of `>>=`  has the type `Monad m => a -> m b`.
So for lists, the type is `a -> [b]`.
For functions, the type is `a -> (r -> b)`, or `a -> r -> b`.
In other words, it's a binary function.
So monad bind on two functions turns a binary function into a unary function with the result of the first function passed as the first argument.

## `Control.Monad.State`

Manipulating stateful computations can be tedious in Haskell because nothing can be changed by side-effect.
You have to pass the next state out of the function along with whatever result.
We can model a stateful computation as returning a pair `(a, s)` where `s` is the type of the state.

If we have a bunch of functions of the type `s -> (a, s)`, we can chain them together with `>>=` and take the tedium out of unpacking the state at each step.
`Control.Monad.State` is the monad instance for this:

```hs
newtype State s a = State { runState :: s -> (a, s) }
```

So, `State` is actually the type of a function.

## Using `Either` to propagate errors

`Maybe` lets us perform computations that return nothing, but it's not good for representing errors because there's no information associated with `Nothing`.
We can use `Either` for this instead.
According to the monad implementation for `Either`, a `Left` value indicates a failure while `Right` indicates success.

## Monadic functions

`Control.Monad` defines the following functions:

```hs
liftM
ap
join
filterM
foldM
```

`liftM` works just like `fmap`, but for monads:

```hs
> :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b

> :t liftA
liftA :: Applicative f => (a -> b) -> f a -> f b

> :t liftM
liftM :: Monad m => (a -> b) -> m a -> m b
```

But since all monads must also be functors, it doesn't really matter.

Also, the `ap` function from `Control.Monad` is just like `<*>`:

```hs
> :t (<*>)
(<*>) :: Applicative f => f (a -> b) -> f a -> f b

> :t ap
ap :: Monad m => m (a -> b) -> m a -> m b
```

It seems that the reason for this is that, in older versions of Haskell, `Monad` instances were not actually constrained to be `Applicative` or `Functor` instances.

If you'e defining your own monad instance, it may be easier to define everything in terms of monad operations and then just define `pure = return` and `(<*>) = ap`.

The `join` function "flattens" a double monad:

```hs
> join [[1], [2, 3]]
[1,2,3]
```

The `filterM` function is a "generalization of the `filter` function for monads.
The `M` suffix means that all function results now return `m a` instead of just `a`:

```hs
filter  ::            (a ->   Bool) -> [a] ->   [a]
filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
```

So, it takes a predicate that returns a monadic `Bool`.

I played aroudn with it a bit.
I don't quite get how it works, though.
When `m = []`,

```hs
length $ filterM (\x -> ys) xs = (length ys) ^ (length xs)
```

To illustrate, *LYH* gives the example of building the power set of a list (note that `Data.Set` already contains a `powerSet` function):

```hs
powerSet = filterM (const [True, False])
```

Like `filterM`, `foldM` is the monadic generalization of `foldl`:

```hs
foldl ::  Foldable t           => (b -> a ->   b) -> a -> t a ->   b
foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> a -> t a -> m b
```

One situation where both of these functions are useful is using a `Writer` to log intermediate results.