# Syntax in Functions

## Pattern matching

Pattern matching looks like defining functions for different fixed values and then a variable last:

```
englishNumber :: (Integral a) => a -> String
englishNumber 0 = "zero"
englishNumber 1 = "one"
englishNumber 2 = "two"
englishNumber 3 = "three"
englishNumber 4 = "four"
englishNumber 5 = "five"
englishNumber n = "Not between 0 and 5"
```

The first matching pattern wins, which is why `englishNumber x` goes last.

The compiler won't make you match every case.
If no pattern matches the input, you'll get an exception at runtime.

You can also pattern match to decompose lists and tuples:

```
addPairs (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

head' []    = error "empty list"
head' (x:_) = x
```

In the `head'` function, notice:
- `_` is used as the "discard" identifier to avoid binding a name that won't be used
- The `error` function raises an `Exception` with the given message
- You can use `:` in patterns, but you can't use `++` because it's ambiguous.

Since function application has higher precedence than any operator, we need the parentheses around `:`.

You can also pattern match inside list comprehensions:

```
let xs = zip [1..10] $ repeat 4
[a + b | (a, b) <- xs]
```

If you want to keep a reference to the whole list while matching elements, you can use `@`, like `xs@(x:_)`.

## Guards

While patterns work simply by checking for equality, **guards** work on any kind of boolean expression.

```
englishNumber' :: Integral a => a -> String
englishNumber' n
    | n == 0    = "zero"
    | n == 1    = "one"
    | n == 2    = "two"
    | n == 3    = "three"
    | n == 4    = "four"
    | n == 5    = "five"
    | otherwise = "Not between 0 and 5"
```

`otherwise` is defined as `True` and exists just for readability in guards.

## Case expressions

`case` expressions are Haskell's version of `switch-case` statements from C-like languages:

```
englishNumber'' :: Integral a => a -> String
englishNumber'' n =
    case n of
        0 -> "zero"
        1 -> "one"
        2 -> "two"
        3 -> "three"
        4 -> "four"
        5 -> "five"
        otherwise -> "Not between 0 and 5"
```

They also work with patterns and are an alternative to pattern matching in function definitions.
Being expressions, they also work wherever an expression is used.

## `where` and `let`

You can always just define functions in the module scope for everything, but Haskell provides a couple of syntactic constructs for scoping names.

`where` clauses are used to define a function inside another function:

```
quadratic a b c = ((-b) + sqrt determinant) / (2 * a)
    where determinant = (b ^ 2) - (4 * a * c)
```

`let ... in` expressions are used to define a function for a single expression (the `in` clause).
The whole `let ... in` expression evaluates to the value of the `in` expression with the substitutions applied.

```
4 * (let a = 9 in a + 1) + 2
```

Note that you can also use pattern matching in `where` and `let` to assign multiple values.