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
