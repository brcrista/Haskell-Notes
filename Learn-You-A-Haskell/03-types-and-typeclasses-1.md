# Types and Typeclasses

The `:t` command in GHCi will show you the type of an expression.

## Common types

- `Int`: bounded integer
- `Integer`: "big int" / unbounded integer
- `Float`: single-precision floating-point
- `Double`: double-precision floating-point
- `Bool`
- `Char`
- `String` (= `[Char]`)

## Type variables

If we look at the type of the `head` function, we get:

```hs
> :t head
head :: [a] -> a
```

The `a` here is a **type variable**.
This is similar to a type parameter in C#.
In Haskell, all functions are "generic" by default.
When an argument is given, a more specific type is inferred for the expression.

## Explicit and implicit typing

*Learn You a Haskell* recommends putting an explicit type on all your functions.
The compiler will often infer a more generic type for you, which I think is desirable in a lot of cases.

For example, consider the code for Fizzbuzz with the type annotations I would choose:

```hs
divisible :: Int -> Int -> Bool
divisible a b = a `mod` b == 0

fizzBuzzInner :: Int -> String
fizzBuzzInner n =
    if n `divisible` 3 && n `divisible` 5
        then "fizzbuzz"
    else if n `divisible` 3
        then "fizz"
    else if n `divisible` 5
        then "buzz"
    else
        show n

fizzBuzz :: Int -> [String]
fizzBuzz n = map fizzBuzzInner [1..n]
```

However, if you leave it to the compiler, it will infer more general types:

```hs
> :t divisible
divisible :: Integral a => a -> a -> Bool

> :t fizzBuzzInner
fizzBuzzInner :: (Integral a, Show a) => a -> [Char]

> :t fizzBuzz
fizzBuzz :: (Integral a, Show a) => a -> [[Char]]
```

On the other hand, you might want to use the type system to prevent certain error cases.
Consider a definition for the factorial function:

```hs
factorial 0 = 1
factorial n = n * (factorial $ n - 1)
```

Without the annotation `factorial :: Integral a => a -> a`, the compiler will infer `Num` here and there's nothing to stop you from saying `factorial 0.5`.

### Monomorphism restriction

Another reason to provide an explicit type is the **monomorphism restriction**.
*Monomorphism* is the antonym of *polymorphism*.
In some cases, the compiler will actually infer a *less* generic type.
For example, the function

```hs
plus = (+)
```

will get inferred as

```hs
plus :: Integer -> Integer -> Integer
```

instead of

```hs
plus :: Num a => a -> a -> a
```

Note, though, that the monomorphism restriction is turned off by default in GHCi.

For more information, see <https://wiki.haskell.org/Monomorphism_restriction>.

## Typeclasses

The `(Integral a, Show a)` to the left of the `=>` in those function types are **typeclasses**.
A typeclass is kind of like an interface in Java or C#.
It isn't a specific type in its self (i.e. a concrete type), but represents any type that defines certain properties.

Some common typeclasses:
- `Eq`: `==`, `/=`
- `Ord` (implies `Eq`): `compare`
- `Show`: `show`
- `Read`: `read`
- `Enum`: `succ`, `pred`
- `Bounded`: `minBound` / `maxBound`
- `Num` (implies `Show` and `Eq`)

### Type annotations

Consider what happens if you try to use `read` in GHCi:

```hs
> read "1"
*** Exception: Prelude.read: no parse
```

Adding a **type annotation** will work though:

```hs
> read "1" :: Int
1
```

Consider the type of `read`:

```hs
> :t read
read :: Read a => String -> a
```

You can think of `Read a` as an open generic type.
If you say `read "1"` by itself, the compiler can't infer what the return type is supposed to be.
You can see what happens if you provide different annotations:

```hs
> read "1" :: Float
1.0
> read "1" :: Integer
1
```

### Numeric types

Haskell has an intricate numeric type hierarchy that includes both typeclasses and types.
The `Num` typeclass is the top of the hierarchy.
It basically means, "able to act like a number."

```hs
> :t 5
5 :: (Num t) => t
```

Woah -- it's a "generic" literal!
The arithmetic operators are likewise generic:

```hs
> :t (+)
(+) :: Num a => a -> a -> a
```

Here's a simple hierarchy of numeric types.
The leaves are types and everything else is a typeclass.

```
Num
    Integral
        Integer
        Int
    Floating
        Float
        Double
```

Unlike C and its descendants, Haskell won't coerce integral numeric types to floating-point types.
For this, you can use the `fromIntegral` function:

```hs
> :t fromIntegral
fromIntegral :: (Integral a, Num b) => a -> b
```