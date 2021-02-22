# Algebraic data types

The ways to combine data constructors form an algebra on the set of types.

## Sum types

A `data` declration like

```hs
data StringOrBool = S String | B Bool
```

is called a **sum type** or an **or type**.
If we equate types with sets, then the sum operation on types is equivalent to set union.

We can define a generic sum type:

```hs
data Sum a b = Type1 a | Type2 b
```

But this is isomorphic to the `Either` type constructor defined in `Prelude`:

```hs
data Either a b = Left a | Right b
```

If we look at the types as sets and use `|S|` to denote the cardinality of the set `S`, then `|Sum a b| = |a| + |b|`.

## Product types

A `data` declaration like

```hs
data StringAndBool = SB String Bool
```

is called a **product type** or an **and type**.
If we equate types with sets, then the sum operation on types is equivalent to the Cartesian product.

We can define a generic product type:

```hs
data Product a b = Product a b
```

But this is isomorphic to the built-in type constructor `(,)`.

Records are another way to make product types with built-in functions for accessing (called **projecting**) the data.

As with sum types, product types are so named because `|Product a b| = |a| * |b|`.

## The void and unit types

Algebras need **identity elements**.
For numbers, the identity element for addition is 0 and for multiplication it is 1.
The identity type for sum types is `Data.Void`, which is a type with no values.
The identity type for product types is `()`, the unit type, which is a type with one value.

Now, the values of the type `(Int, ())` are not equal to values of the type `Int`, but the two types are isomorphic.

`Void` is also the zero value for product types, following the equation `|Product a b| = |a| * |b|`.
Because `Void` has no values, `|Product a Void| = |a| * |Void| = |a| * 0 = 0`.
Therefore, it's not possible to construct an actual value of type like `Product Bool Void`.

However, it is possible to declare expressions with this type, but the expression cannot be fully evaluated.
For example, we can do this just fine:

```hs
> x = (True, undefined) :: (Bool, Void)
> fst x
True
```

but calling `print x` would throw an exception.

Also note that any number of units in a product type is still just isomorphic to unit:

```hs
Data Twonit = ((), ())
```

So if `Void` corresponds to 0 and `()` corresponds to 1, what corresponds to 2?
Well, that's the sum type of unit plus unit:

```hs
data Bool = True () | False ()
```

But since there's only one possible value for `()`, that's isomorphic to

```hs
data Bool = True | False
```

## Lists as an algebraic type

The power of algebraic types is that they give us a language to describe other types.
Specifically, we can manipulate *equations* of types.

For example, consider the definition of the list type:

```hs
data [a] = [] | a : [a]
```

This will not compile, but we could write it in valid Haskell like

```hs
data List a = Empty | Cons a (List a)
```

Now, the empty list is isomorphic to unit; it has only one possible value.
`Cons` is a product type.
It's also recursive and is defined in terms of `List a`.
We can map this type definition to the equation:

```js
List(a) = 1 + a * List(a)
```

While we don't actually have subtraction and addition defined for types, we could still manipulate this algebraically to solve for `List(a)` and verify that the solution works for types as well.
We could also solve this by using a **fixed point**, or recursively substituting the RHS of the equation for `List(a)` and collecting the terms.
Either way, the solution to this equation is actually the geometric series:

```js
List(a) = 1 + a + a^2 + a^3 + ...
```

If we convert back to the type language, this is:

```hs
List a = () | (a) | (a, a) | (a, a, a) | ...
```

Haskell has no way to represent an infinite sum type like this, but we can see that the recursive definition is equivalent!