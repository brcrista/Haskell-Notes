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