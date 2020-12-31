# Input and Output

## Unit, the `IO` type, and the `main` function

The function `putStrLn` has the following type:

```
> :t putStrLn
putStrLn :: String -> IO ()
```

The empty tuple there is also called **unit**.
The reason for this name is that it only has one possible value.
The `IO` type constructor is used to denote an expression that will perform I/O when evaluated.
A function named `main` is special in that it will be evaluated when the program is compiled into an executable and run.
It is also required to have a type `IO a` for some `a`.

By the way, it's conventional not to put an explicit type on `main`.

## `do` syntax

`do` allows you to write code in the procedural style in the `IO` context:

```
main = do
  _ <- putStrLn "What's your name?"
  name <- getLine
  putStrLn ("Hello, " ++ name ++ "!")
```

Since the first line discards its result (which is just `()`), Haskell also lets us write:

```
main = do
  putStrLn "What's your name?"
  name <- getLine
  putStrLn ("Hello, " ++ name ++ "!")
```

However, note that the last line in a `do` block **cannot** be bound to a name.

The type of `getLine` is

```
> :t getLine
getLine :: IO String
```

The `<-` "operator" unpacks the `String` from the result of `getLine` and assigns it `name`.
So, we can make two generalizations:
1. You can only receive input in an `IO` context
2. You can call non-`IO` functions from inside the `IO` context

Code that doesn't perform I/O is called **pure**.
While you could just give up and make everything `IO`, Haskell encourages you not to by making I/O a little bit uglier.