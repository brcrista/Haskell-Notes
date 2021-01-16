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