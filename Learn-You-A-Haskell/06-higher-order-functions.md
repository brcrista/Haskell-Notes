# Higher-Order Functions

## Currying

All functions in Haskell are **curried** by default.
Calling a function with fewer arguments than parameters is called **partial application**.

(This is not to be confused with a **partial function**, which is a function that doesn't accept arguments of every value in its types. For example, `div` and `head` are partial functions.)

By the way, you can still write functions in the "normal" way and they won't be curried:

```
> let add(x, y) = x + y
> :t add
add :: Num a => (a, a) -> a

> add(1, 2)
3
```

This is just using a tuple for the first parameter.

The `curry` and `uncurry` functions convert between these forms.

Identities from <https://wiki.haskell.org/Currying> (exercises):

```
(,) = curry id
fst = uncurry const
snd = (uncurry . flip) const
swap = (uncurry . flip . curry) id
```

### Advantages of currying

* Free partial application on any function
* Define functions as point-free
* If a function takes another function, you include that function's parameters as parameters to the original function (consider `flip' f x y = f y x`)
* Combinators can treat any function as unary (?)

## Lambdas

A **lambda** is an anonymous function.
Lambdas are created with the syntax

```
\x y -> f x y
```

Lambdas often need to be surrounded by parentheses when they don't extend all the way to the end of the line.

## Folds

A **fold** is a function that computes a single value from a list of values.
These could be computed with explicit recursion, but the case is common enough that the concept of folds is helpful.

There are several flavors of folds.
`foldl` takes a function, a starting value, and a list to apply it to:

```
> :t foldl
foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
```

In C# we might express this signature as:

```cs
TResult FoldL(Func<TResult, T, TResult>, TResult, Foldable<T>)
```

The second parameter that has the same type as the result is called the **accumulator**.
The function takes a value of the accumulator and a value of the list.
It calls the function on these two values and the result is the next value for the accumulator.
At the end, the final accumulator value is returned.

Here's an implementation of `sum` using `foldl`, which folds from the left:

```
sum' xs = foldl (+) 0 xs
```

In this case, we can use `foldl1`, which just uses the first element for the initial value of the accumulator:

```
sum' xs = foldl1 (+) xs
```

In general, we can perform this simplification whenever the initial value of the accumulator is the zero value of the function.

One final simplification we can make is to take advantage of partial application and define the function in **point-free** style:

```
sum' = foldl1 (+)
```

Notice that we end up with the same function type as the built-in `sum`:

```
> :t sum
sum :: (Num a, Foldable t) => t a -> a

> :t sum'
sum' :: (Num a, Foldable t) => t a -> a
```

In fact, we could even implement `map` and `filter` with folds.

While we used `foldl` here, we could just as easily use `foldr` since addition is associative.

### Scans

You can think of folds as producing a list of accumulators mirroring the input list.
This is how the **scan** functions work (`scanl`, etc.).
They return the whole list of computed accumulators.

## The $ and . operators

The `$` operator simply applies a function, but has the lowest precedence of any operation.
So, its main use is for getting rid of pairs of parentheses.

```
> length $ scanl1 (+) [1..10]
10
```

The `.` operator is the **function composition** operator.
In Haskell, we use the definition of function composition that `g` is called first, then `f`:

```
(f . g) x = f $ g x
```

The types of these operators are:

```
> :t ($)
($) :: (a -> b) -> a -> b

> :t (.)
(.) :: (b -> c) -> (a -> b) -> (a -> c)
```

so

```
> t: flip (.)
flip (.) :: (a -> b) -> (b -> c) -> (a -> c)
```

Also, note that `($) f` is equivalent to `id f`.

## Type checking function calls

It's worth explaining how to evaluate the types of expressions so you can understand what will and will not compile.
(Note: this is my own process and I'm not sure if this is exactly what the compiler does.)

Given a function call `f x`, use these steps to determine the type of the expression:
1. **Equate:** Match the type variables for the argument in the function's type (LHS of the leftmost, topmost `->`) to the type variables in the argument's type.
2. **Substitute:** Replace the argument type variables in the function type using the equation from (1). This can be skipped when there aren't any constraints.
3. **Evaluate:** Remove the argument type from the expression (LHS of the leftmost, topmost `->`).
4. **Simplify**: Use the equation from (1) to express the function's type using the fewest possible type variables. Remove unnecessary parentheses. Rename as necessary.

Since all functions are curried, the above steps can just be repeated for every argument.

For the following examples, refer to these types:

```
'A' :: Char
1 :: Num a => a

id :: a -> a
const :: a -> b -> a
curry :: ((a, b) -> c) -> (a -> b -> c)
uncurry :: (a -> b -> c) -> ((a, b) -> c)
flip :: (a -> b -> c) -> (b -> a -> c)
(.) :: (b -> c) -> (a -> b) -> (a -> c)
```

```
> :t id 'A'

-- Equate
a = Char

-- Substitute
Char -> Char

-- Evaluate
id 'A' :: Char
```

```
> :t const 1

-- Equate
a = Num a_arg => a_arg

-- Substitute
Num a_arg => a_arg -> b -> a_arg

-- Evaluate
const 1 :: Num a_arg => a_arg -> b -> a_arg

-- Simplify
-- Since there is no type variable named `a` anymore, replace `a_arg` with `a`
const 1 :: Num a => b -> a
```

```
> :t curry id

-- Equate
(a, b) -> c = a_arg -> a_arg

-- Substitute
-- Evaluate
curry id :: a -> b -> c

-- Simplify
(a, b) = a_arg = c
curry id :: a -> b -> (a, b)
```

```
> :t uncurry const

-- Equate
a -> b -> c = a_arg -> b_arg -> a_arg

-- Substitute
-- Evaluate
uncurry const :: (a, b) -> c

-- Simplify
a = a_arg = c
uncurry const :: (a, b) -> a
```

```
> :t (uncurry . flip) const

-- (uncurry .)
-- Equate
b -> c = (a_arg -> b_arg -> c_arg) -> ((a_arg, b_arg) -> c_arg)

-- Substitute
-- Evaluate
((a_arg, b_arg) -> c_arg)) -> (a -> b) -> (a -> c)

-- Simplify
b = (a_arg -> b_arg -> c_arg)
c = ((a_arg, b_arg) -> c_arg)
((a_arg, b_arg) -> c_arg)) -> (a -> (a_arg -> b_arg -> c_arg)) -> a -> (a_arg, b_arg) -> c_arg

-- rename a     as a1
-- rename a_arg as a2
-- rename b_arg as b
-- rename c_arg as c

(uncurry .) :: (a1 -> a2 -> b -> c) -> a1 -> (a2, b) -> c

-- (uncurry . flip)
-- Equate
(a1 -> a2 -> b -> c) = (a_arg -> b_arg -> c_arg) -> (b_arg -> a_arg -> c_arg)

-- Substitute
-- Evaluate
(uncurry . flip) :: a1 -> (a2, b) -> c

-- Simplify
a1 -> a2 -> b -> c = (a_arg -> b_arg -> c_arg) -> b_arg -> a_arg -> c_arg
a1 = (a_arg -> b_arg -> c_arg)
a2 = b_arg
b = a_arg
c = c_arg

(a_arg -> b_arg -> c_arg) -> (b_arg, a_arg) -> c_arg
-- Drop the `_arg`
(uncurry . flip) :: (a -> b -> c) -> (b, a) -> c

-- (uncurry . flip) const
-- Equate
a -> b -> c = a_arg -> b_arg -> a_arg

-- Substitute
-- Evaluate
(uncurry . flip) const :: (b, a) -> c

-- Simplify
a = a_arg = c
(b, a) -> a
-- Swap a and b
(uncurry . flip) const :: (a, b) -> b
```