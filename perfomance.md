# Performance

Haskell being a lazy, functional, garbage-collected language can make it difficult to reason about how much memory you're using.

If you think reasoning about memory usage with garbage collection is difficult, just wait until thunks are in play.

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

## Strictness

(From <https://www.fpcomplete.com/haskell/tutorial/all-about-strictness/>)

By default, all expressions in Haskell are lazy.
We just build up a chain of computations and nothing will be computed until we force evaluation (such as by printing the result to the console).

A **thunk** is a data structure in the language runtime that forms a link in this chain of computation.
If you think of a thunk as a closure / lambda, that's the basic idea.
A thunk contains:
- Some metadata identifying it as a thunk
- A pointer to the function it calls
- Pointers to each of the arguments to the function

Of course, those arguments -- and even the function itself -- may be thunks too!
But it should be clear that a thunk uses much more memory than the actual result of the function.

### Bang patterns and `seq`

Sometimes, laziness will just balloon our memory usage to unacceptable levels and we need to put some caps on it.

The function

```hs
add x y = x + y
```

will be evaluated lazily by default.
But if you do:

```hs
add !x !y = x + y
```

This will force both of its arguments to be evaluated before the function is called, thereby making it strict.
In other words, it works just like a C function now.
The terms `!x` and `!y` are called **bang patterns**.
Though of course, if `add` itself doesn't have evaluation forced on it, the strictness of its arguments won't matter.

This is actually syntactic sugar for

```hs
add x y = seq x (seq y (x + y))

-- Or, more idiomatically:
add x y = x `seq` y `seq` x + y
```

`seq` is a special function that uses compiler primitives to force evaluation of its first argument when its second argument is evaluated.
So, you "yoke together" the two expressions with `seq`.
Note in the above example that `x + y` is the innermost / rightmost expression.
To get to it, you have to evaluate `x` and `y` first.

These details aren't too important, and using bang patterns is usually easier to read.
Bang patterns are actually a language extension and you need the pragma

```hs
{-# LANGUAGE BangPatterns #-}
```

to use them.

By the way, bang patterns can be used on the LHS of `let` bindings as well.
This is a useful trick to introduce bang patterns where you need them.
However, you may find yourself making your code more verbose to accommodate strictness declarations: `let c = a + b; let d = a + c`.
It starts to look more like imperative code.

**Note:** the order of evaluation is *not* guaranteed in a sequence of bang patterns.

### `Control.DeepSeq`

Using `seq` / bang patterns does not actually force the whole tree of computations in a thunk to be evaluated.
It actually only evaluates as much as it needs to perform the computation:

```hs
> putStrLn $ undefined `seq` "Hello World"
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:3:12 in interactive:Ghci2

-- Note the `const undefined`
> putStrLn $ const undefined `seq` "Hello World"
Hello World
```

The technical term for this is that it evaluates to "weak-head normal form" (WHNF).
To force *all* thunks to be evaluated -- what is called just "normal form" (NF), we need to use a function called `deepseq`.
This function lives in `Control.DeepSeq` (this module is technically non-standard but included in GHC).

### Strict data

Besides putting strictness annotation on a variable name, you can put them on data types:

```hs
data RunningTotal = RunningTotal
  {
    sum   :: !Int,
    count :: !Int
  }
```

This means that the fields in the record will be strictly evaluated.
This can go a long way towards avoiding big trees of thunks.
It's also standard Haskell -- you don't need the `BangPatterns` extension.

### The `$!` operator

The `$!` operator (in Prelude) is like `$` except it forces strict evaluation of its RHS:

```hs
($!) :: (a -> b) -> a -> b
f $! x = x `seq` f x
```

In `Control.DeepSeq` we have `$!!`, which does the same thing except using `deepseq`.
We also have `force`, which evaluates an expression in WHNF to NF.
So, `f $!! x = f $! force x`.

### `Data.Map.Lazy` and `Data.Map.Strict`

`Data.Map` has two flavors: a lazy implementation and a strict implementation.
Some other data structures follow this pattern.

Both types are strict in their keys, but `Data.Map.Strict` will force evaluation of values before they are added to the map while the `Lazy` map won't.
`Data.Map.Lazy` is the default exported by plain `Data.Map`.

### `foldl`

`foldl` is notorious for consuming lots of memory by building up a long chain of thunks before evaluating anything.
If space is a concern, always use `Data.List.foldl'`.
Despite being in the `Data.List` module, it will still work on any `Foldable`.