# Starting Out

## Arithmetic

The only "weird" stuff is:
- Negative numbers will cause compiler errors unless you surround them with parentheses
    - ex. `5 * (-3)`
- `&&` and `||` are the boolean operators, but `not` is a prefix function (not `!`)
- `/=` is the inequality operator
- `^` can be used for exponentiation

## Functions

Functions look just like variables:

```hs
add a b = a + b
```

Note that in GHCi, you have to use `let`:

```hs
let add a b = a + b
```

Note that `'` is a valid character in a function name.
It conventionally denotes a "strict" (eager) version of a function.
It can also mean "prime" as in mathematics.

### Calling functions

Functions are called without parentheses:

```hs
add 1 2
```

Also, any function can be called as an **infix function** by surrounding it with backticks:

```hs
1 `add` 2
```

### Operators

A function whose name consists only of special characters is called an **operator**.
Operators are called with infix syntax by default.
To call an operator with prefix syntax or pass it to another function, surround it with parentheses.

```hs
> (+) 1 2
3
```

You can change the precedence rules for an operator with a **fixity declaration**.
This says whether an operator is non, left or right-associative (`infix`, `infixl`, `infixr`) and its precedence level (0-9).
Function application always has highest precedence (fixity 10).

```hs
infixr 5 ++ -- define the `++` operator to be right-associative with a precedence level of 5
```

### Some built-in functions

- `not`
- `succ`
- `mod`
- `min` / `max`
- `compare`
- `show` / `read`

## `if` expression

```hs
if x > 100 then x else x * 2
```

## Referential transparency

Haskell is said to be **referentially transparent.**
This means that (pure) functions always produce the same result for the same inputs.
This has three important consequences:

1. You can reason about programs by just replacing functions with their definitions as in algebra
1. It doesn't matter when a function is evaluated
1. The results of functions can be cached or memoized without worrying about the results becoming stale

## Laziness

- <https://wiki.haskell.org/Lazy_vs._non-strict>
- <https://wiki.haskell.org/Seq>
- <https://wiki.haskell.org/Performance/Strictness>
- <https://stackoverflow.com/questions/7396978/left-and-right-folding-over-an-infinite-list>

Haskell is often described as **lazy**, which means that expressions are not evaluated until they are needed.
Of course, this is a natural property of functions in most programming languages.
So if everything is a function, then everything is lazy!
However, the language spec only says that Haskell "non-strict."
That is, it doesn't *guarantee* laziness, it just doesn't guarantee strictness.

**Strict** evaluation is the "inside-out" evaluation familiar from languages such as C and Python.
When you call a function, the arguments are evaluated first, and then the function is called.
**Lazy** evaluation is "outside-in" evaluation.
C's "short-circuiting" `&&` and `||` operators are familiar examples of lazy evaluation.
Also, if you just make all variables into functions / lambdas in a language like JavaScript and call those functions when you need the results, that would be lazy too.

For example, in the evaluation of `a + b * c`:
- Strict: `b * c` first, then `a + _`
- lazy: `a + _` first, then `b * c`

Of course, the lazy evaluation works in Haskell because of currying.

## Lists

- Lists in Haskell are backed by linked lists.
- Strings are just lists of characters. This works because lists in Haskell are already immutable.
- Because Haskell is lazy, lists can be infinite!
    - Other languages need iterators to do this.

### List operators

- The `++` operator concatenates lists.
- The `:` ("cons") operator prepends an element to a list.
- `!!` gets an element out of a list by index.
- The equality and comparison operators perform element-by-element comparisons on lists.

### List functions

- `head` / `tail`
- `init` / `last`
- `length`
- `null` (use this instead of `xs == []`)
- `elem` ("contains")
- `reverse`
- `take` / `drop`
- `minimum` / `maximum`
- `sum` / `product`
- `cycle` / `repeat` / `replicate`
- `zip`

### Ranges

Range syntax will create lists of integers or strings:

```hs
[1..20]
['a'..'z']
```

We'll see later that any type that implements the `succ` and `pred` functions can be used in a range (i.e., the `Enum` typeclass).
You can technically do floating-point numbers as well, but you might not get what you expect.

If you leave off the upper bound, you will get an infinite list:

```hs
[1..]
```

### List comprehensions

```hs
[x * 2 | x <- [1..10]]
[x * 2 | x <- [50..100], x `mod` 7 == 3]
```

We can include several predicates, separated by commas.
I'm not sure if there's any difference here from just using `&&`.

```hs
[x | x <- [10..20], x /= 13, x /= 15, x /= 19]
```

We can also draw elements from multiple lists:

```hs
[x * y | x <- [1..3], y <- [-1..1]]
```

## Tuples

**Tuples**, unlike lists, can contain different types of elements.
The order and number of element types in the tuple determine the tuple's type.

```hs
(1, 2, "hello") -- this is a tuple
```

The `fst` and `snd` functions only work on pairs.