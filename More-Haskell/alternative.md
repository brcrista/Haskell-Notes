# Alternative

<https://www.youtube.com/watch?v=ColY2dknvJs>

`Alternative` lives in the `Control.Applicative` package.

`Alternative` is a monoid on an applicative functor:

```hs
-- Not the real definition, just illustrative
class Monoid a where
  mempty :: a
  (<>) :: a -> a -> a
```

we have

```hs
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
```

Its laws are the same as `Monoid`'s.

It is commonly used to represent an operation that can fail with a fallback.

The most obvious `Alternative` is `Maybe`, where `<|>` chooses the first non-empty value.

`Alternative` is also used in parsers, where it lets you try multiple parses until one succeeds.

`Alternative` comes with a couple other functions that have default implementations:

```hs
some :: f a -> f [a]
many :: f a -> f [a]
```

These each repeat an operation until it fails and then returns a list of the results.
However, if the first result is a failure, `many` will return `pure []` while `some` will return `empty`.
For that reason, `some` will always return a nonempty list.

These functions are most useful for operations that have some side effect, like doing I/O or updating some state.