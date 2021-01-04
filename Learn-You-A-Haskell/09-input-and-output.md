# Input and Output

## Unit, the `IO` type, and the `main` function

The function `putStrLn` has the following type:

```hs
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

```hs
main = do
  _ <- putStrLn "What's your name?"
  name <- getLine
  putStrLn ("Hello, " ++ name ++ "!")
```

Since the first line discards its result (which is just `()`), Haskell also lets us write:

```hs
main = do
  putStrLn "What's your name?"
  name <- getLine
  putStrLn ("Hello, " ++ name ++ "!")
```

However, note that the last line in a `do` block **cannot** be bound to a name.

The type of `getLine` is

```hs
> :t getLine
getLine :: IO String
```

The `<-` "operator" unpacks the `String` from the result of `getLine` and assigns it `name`.
So, we can make two generalizations:
1. You can only receive input in an `IO` context
2. You can call non-`IO` functions from inside the `IO` context

Code that doesn't perform I/O is called **pure**.
While you could just give up and make everything `IO`, Haskell encourages you not to by making I/O a little bit uglier.

## `Control.Monad`

The `Control.Monad` module has some handy functions for flow of control.
Since Haskell is non-strict, you can define ordinary functions that work like `if`, `for`, and `while` in C-like languages:


```hs
forM
forever
when
```

## `System.IO`

Functions and types for reading and writing files are in the `System.IO` module.

```hs
BufferMode
FilePath
Handle
IOMode
appendFile
hClose
hFlush
hGetChar
hGetLine
hGetContents
hPrint
hPutChar
hPutStr
hPutStrLn
hSetBuffering
openFile
openTempFile
readFile
withFile
writeFile
```

Note that functions that read from a file such as `hGetContents` will stream the file rather than loading all of its contents into memory.
By default, this is by line, but you can set it with `hSetBuffering` and `BufferMode`.

## `System.Directory`

```hs
getCurrentDirectory
removeFile
renameFile
```