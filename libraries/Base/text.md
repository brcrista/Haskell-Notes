# Text

## `Data.Text`

`Data.Text` is technically nonstandard, but it ships with GHC.
The `Text` type is like `String` except more efficient.
Recall that a `String` is just an alias for `[Char]` -- a linked list of (Unicode) characters.
`Text`, on the other hand, uses an array internally.
Also, `Data.Text` contains a set of `Text`-specific functions that are more efficient than their list-based counterparts.
Specifically, they enable a compiler optimization called **fusion**.
Fusion means that, in a sequence of function calls, unneeded intermediate `Text` values are optimized away and only a single allocation is made for the result.

You can convert a `String` to and from a `Text` using the `pack` and `unpack` functions, respectively.
If you're using `Text`, it's usually convenient to enable the `OverloadedStrings` and `ExtendedDefaultRules` compiler extensions as well.

Some people advocate just using `Text` in all cases, but `String` is perfectly fine for normal use (much like choosing between `ArrayList<T>` and `LinkedList<T>` in Java).
`Text` is probably better if you are loading large files into memory or doing any kind of localization.

## Issues with file I/O

There's a well-known gotcha (see [this](https://www.snoyman.com/blog/2016/12/beware-of-readfile/) and [this](https://stackoverflow.com/questions/33444796/read-file-with-utf-8-in-haskell-as-io-string)) with Haskell's `readFile` function and its relatives.
Instead of just defaulting to UTF-8 or taking an encoding as an input, it will try to guess an encoding based on environment variables such as `$LANG`.
This really comes up as a problem when you're distributing code such as a command-line tool that will run on a variety of platforms.

Also, the default-lazy / streaming I/O functions seem to be a bad fit for production code, where I/O is extremely important for resource usage.
When this is a concern, Michael Snoyberg [recommends using the RIO library](https://www.snoyman.com/blog/2020/10/haskell-bad-parts-1/), which contains a function `readFileUtf8`.
[Elsewhere](https://www.snoyman.com/blog/2016/12/beware-of-readfile) he recommends using `Data.ByteString.readFile` to read small files strictly or using the `conduit` library for streaming.