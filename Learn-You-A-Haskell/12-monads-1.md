# Monads

Monads, like functors and applicative functors, provide a way to apply a function to a value in some context.

## The `Monad` typeclass

Here is the `Monad` typeclass:

```hs
class Applicative m => Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b
    (>>)   :: m a -> m b -> m b
```

Note that, to be a monad, a type must also be an applicative functor.
`return` works just like `pure` and even has the same signature except for the type constraint.
The `>>=` operator represents the **monad bind** operation.
This is the key function for monads, and `>>` is just defined in terms of `>>=`.

Like `fmap`, the monad bind operation applies a function to a value inside a context.
However, in this case, the function returns its value in the same context.
If we just used `fmap`, we would end up with `m m b`.
But `>>=` "flattens" out the result to just `m b`.

### Functors, applicative functors, and monads

Let's compare the operations we have:

```hs
> :t (<$>)
(<$>) :: Functor f => (a -> b) -> f a -> f b

> :t (<*>)
(<$>) :: Applicative f => f (a -> b) -> f a -> f b

> :t (>>=)
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

If we just use `Monad` for the type constraint (since monads are also functors and applicative functors and use `(=<<) = flip (>>=)`, we can see the similarity more clearly:

```hs
(<$>) :: Monad m =>   (a -> b) -> m a -> m b
(<$>) :: Monad m => m (a -> b) -> m a -> m b
(=<<) :: Monad m => (a -> m b) -> m a -> m b
```

The only thing that differs is the type of the function that gets passed.

## Monad instances

### `Maybe` as a monad instance

`Maybe` is frequently used as a monad.
That's because `>>=` gives us a way to link together several operations that each return `Maybe`.
It's the equivalent of doing `if (x == null) return null` after every call in Java or C#.

### `IO` as a monad instance and `do` syntax

`IO` is also commonly used as a monad.
We already saw in chapter 9 how to work with `IO` operations using `do` syntax.
This `do` syntax actually works for *any* monad.

Consider this code:

```hs
let
    x = 3
    y = "!"
in show x ++ y
-- "3!"
```

Now consider doing that with each value in a `Maybe`:

```hs
-- Use some funny indentation to make the point.
Just 3   >>= (\x ->
Just "!" >>= (\y ->
Just $ show x ++ y))
-- Just "3!"
```

The nested lambdas are annoying and not very extensible -- we don't want to end up with a closing parenthesis for every line in the code here.
But, this is basically what `do` syntax is sugar for:

```hs
do
    x <- Just 3
    y <- Just "!"
    Just $ show x ++ y
-- Just "3!"
```

In this way, monads are "programmable semicolons."
For `Maybe`, the "semicolon" is programmed to propagate `Nothing`.

Note that the last line of a `do` block never has `<-` and must evaluate to a monadic value because it's standing in for the body of the lambda.
However, you can have such lines anywhere in the middle of the `do` block as well.
In that case, it's the same as calling `>>` in the middle of the chain.

By the way, if pattern matching fails at some point in a `do` block, the `fail` function is called on the monad.
Some monads will just have this call `error`.
`Maybe` just defines it as `fail = const Nothing`.

## Lists as monads

For lists, `return = (:[])` and `fail = const []`.
The meaning of `>>=` is to "flat map" -- map the function over the list to produce a list of lists and then flatten the result.
