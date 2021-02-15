# `Debug.Trace`

Since Haskell can only perform I/O in certain contexts, how do you perform printf-style debugging?

Enter the `Debug.Trace` module.
It solves that problem by just breaking the rules.
As the name suggests, it is only meant for debugging and should not be shipped in production code.
That's why it's considered ok to break the rules.

The `trace` function takes a string and a value.
It prints the string and then returns the value:

```hs
> trace "1 + 2 = " (1 + 2)
1 + 2 =
3
```

But, if we look at its type signature, there's no `IO` in it.
It's just

```hs
trace :: String -> a -> a
```

That lets us put it anywhere we want.
Under the hood, `trace` uses the function `System.IO.Unsafe.unsafePerformIO` to pull this off.

The `traceShow` function lets us use any instance of `Show` instead of a `String` for the trace message.
This saves us from having to convert it to a string ourselves:

```hs
traceShow :: Show a => a -> b -> b
traceShow = trace . show
```

The `traceId` function is just like `trace` except it just prints its argument and passes it through:

```hs
traceId :: String -> String
traceId a = trace a a
```

It's good for debugging string values.
Likewise, `traceShowId` extends `traceId` to all types by just using its argument's `show` value:

```hs
traceShowId :: Show a => a -> a
traceShowId a = trace (show a) a
```

Now, if you actually *want* to produce an `IO` value, you can:

```hs
traceIO :: String -> IO ()
```

Like any `IO`-returning function, `traceIO` will print its output when its `IO` context is evaluated.
This is useful if you need to sequence the output with some other output coming from somewhere else.