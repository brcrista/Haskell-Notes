# Performance

Haskell being a lazy, functional, garbage-collected language can make it difficult to reason about how much memory you're using.

## Tail recursion

A function is **tail recursive** iff the return value of the function is the result of the recursive call.
For example, the function

```hs
foldl            :: (a -> b -> a) -> a -> [b] -> a
foldl f z []     =  z
foldl f z (x:xs) =  foldl f (f z x) xs
```

is tail recursive.
However, the function

```hs
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

is not because the recursive call has `n *` in front of it.
If we broke it down to the machine code level,
`x = factorial (n - 1)` needs to be evaluated first, and `(*) n x` would be the last thing that gets evaluated.

A common optimization that compilers (for any programming language) will perform is **tail-call optimization**.
Generally, a function call will cause a new stack frame to be created.
With tail recursion, we don't actually need to save any context from the current stack frame, so we can just overwrite it with the new one.
This causes recursive calls to use `O(1)` memory instead of `O(n)`, where `n` is the number of recursive calls.