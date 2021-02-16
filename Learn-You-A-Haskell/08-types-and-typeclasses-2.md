# Making Our Own Types and Typeclasses

## Data constructors

We can define a new type with the `data` keyword:

```hs
data Bool = False | True
```

`False` and `True` here are called **data constructors**.
Data constructors can also take parameters:

```hs
> data Shape = Circle (Float, Float) Float | Rectangle (Float, Float) Float Float
> c = Circle (0, 0) 1
> :t c
c :: Shape
```

If a data constructor takes no parameters, it is said to be **nullary**.
Of course, data constructors are just functions:

```hs
> :t Shape
Circle :: (Float, Float) -> Float -> Shape
```

Any data constructor can be used in pattern matching:

```hs
area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle _ w h) = abs $ w * h
```

By the way, `[]` and `:` are also considered data constructors!
That's why we can use them in pattern matching but not `++` or functions such as `head`.

Note: `Shape` is a type, but `Circle` and `Rectangle` are not!
In fact, a type and one of its data constructors can have the same name.
This is common when a type can only be constructed in one way.

When exporting types from a module, you can choose to export all, some, or none of its data constructors:

```hs
module Shapes(
    Shape (..) -- export all
    Shape (Circle, Rectange) -- export all
    Shape (Cirlce) -- export Circle
    Shape -- export none, need to use some function to create instances
)
```

## Records

**Records** give you a way to define a data constructor and a bunch of functions for operating on it.

So, we could define `Shape` as

```hs
data Shape =
    Circle { center :: (Float, Float), radius :: Float }
    | Rectangle { bottomLeftCorner :: (Float, Float), width :: Float, height :: Float }
```

The data constructors are called in the same way, but then we can also do:

```hs
> width $ Rectangle (0, 0) 5 3
5.0
```

The default implementation of `Show` will also use these names.

## `newtype`

*Note: this is taken from Chapter 11 in the text, but fits better here.*

Consider the following declarations:

```hs
type Name1 = String
data Name2 = Name2 { getName2 :: String }
newtype Name3 = Name3 { getName3 :: String }
```

They'd each be used like:

```hs
> "Brian" :: Name1
"Brian"

> getName2 $ Name2 "Brian"
"Brian"

> getName3 $ Name3 "Brian"
"Brian"
```

The `newtype` keyword can be used in this case where a new type is just wrapping an instance of an old type.
It's a bit faster at runtime than the equivalent `data` declaration.

## Algebraic data types

The ways to combine data constructors form an algebra on the set of types.

### Sum types

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

### Product types

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

### The `Void` and unit types

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

### List as an algebraic type

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

## Type constructors

A type name is actually a constructor too.
It's called a **type constructor**, and we can give it parameters:

```hs
data Maybe a = Nothing | Just a
```

`a` here is called a **type parameter**.
Unlike data constructors, which are evaluated at run time, type constructors are evaluated at compile time through type checking.

As with function types, you can put typeclass constraints on the type parameters of a data declaration.
However, there's a strong convention not to.
Basically, there's no reason a type needs to know about the functions that use it.

A specific issue here is that you still have to put the constraint in functions either way if the function itself relies on the constraint, but not all functions that operate on the data type may actually rely on the constraint.
If you put the constraint on the data declaration, though, you're stuck putting it on all the functions.

## Typeclasses

A type is said to be an **instance** of a typeclass if it implements all of the functions defined for that typeclass.

### Deriving from a typeclass

The easiest way to do this is with the `deriving` keyword in the data declaration.
Haskell can automatically implement the following typeclasses:
- `Bounded`
- `Enum`
- `Eq`
- `Ord`
- `Read`
- `Show`

For example, given

```hs
data Color = Red | Green | Blue deriving (Show, Eq, Enum)
```

we can do:

```hs
> succ Red
Green

> Red == Red
True
```

Note that the implementation of `==` actually checks if the same data constructor was used for the two values as well as the actual fields.

## Defining a typeclass

We can define our own typeclass with the `class` keyword:

```hs
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

    x == y = not (x /= y)
    x /= y = not (x == y)
```

The two operators belonging to `Eq` are defined in terms of each other.
When we go to implement `Eq`, we only need to define one of these operators, and the other will automatically be implmented.

You implement a typeclass like this:

```hs
instance Eq Shape where
    -- Define `==`
```

To make a type constructor produce instances of a typeclass, do:

```hs
instance Eq m => Eq (Maybe m) where
    -- Define `==`
```

Unless the type constructor itself is an instance, as we shall see with `Functor`:

```hs
instance Functor Maybe where
    -- Define `fmap`
```

A typeclass can also be a subclass of another typeclass.
Just add a typeclass constraint as usual: `class Eq a => Num a where`.

If you poke around in GHCi with `:i`, you'll see that a whole lot of the built-in functions are actually defined in typeclasses.
Note that, unlike .NET, you can have non-user-defined types implement typeclasses.

## Type synonyms

A **type synonym** is another name for a type.
The two names are considered equivalent for type checking.

```hs
type String = [Char]
```

Type synonyms can help make our code more self-documenting without the overhead of defining and using new types.

## Functors

We've already seen the `map` function with lists:

```hs
> map (+1) [0..3]
[1,2,3,4]
```

Functors generalize this concept.
If we run `:i Functor` in GHCi, we see its definition:

```hs
class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f
```

The first thing to note is that type constructors can also belong to a typeclass (`f a` and `f b`).

Second, note the similarity of `map` and `fmap`:

```hs
> :t map
map :: (a -> b) -> [a] -> [b]

> :t map
fmap :: Functor f => (a -> b) -> f a -> f b
```

Since `[]` is an instance of `Functor`, it's reasonable to expect that `fmap` for lists behaves the same way as `map` does:

```hs
> fmap (+1) [0..3]
[1,2,3,4]

> fmap (const $ error "kaboom") []
[]
```

However, `fmap` extends to other types besides lists:

```hs
> fmap (+1) (Just 1)
Just 2

> fmap (+1) Nothing
Nothing

> fmap (+1) (Left 1)
Left 1

> fmap (+1) (Right 1)
Right 2
```

`:i Functor` shows the whole list of instances:
- `(->) r`
- `[]`
- `Either a`
- `IO`
- `Map k`
- `Maybe`
- 1, 2, and 3-tuples

One interesting thing here is that `Either a`, `Map k`, and `(->) r` are partially applied type constructors!

Note that `Map k` only appears here if you import `Data.Map`.
`Set` is not a Functor even if you import `Data.Set`, though it does have its own `map` function.
If we look at the type for its `map` function, we see:

```hs
> :t Data.Set.map
Data.Set.map :: Ord b => (a -> b) -> Set a -> Set b
```

The `Ord` requirement keeps it from being an instance of `Functor` since `fmap` has no type constraints.

## Kinds

We've seen that Haskell has both a compile-time type language (types and type constructors) and a run-time computation language (values and functions).
In the run-time computation language, we have types like this:

```hs
> :t True
True :: Bool

> :t (||)
(||) :: Bool -> Bool -> Bool
```

Types and type constructors also have types themselves -- or rather, **kinds**.
Haskell describes the kinds of types with the same syntax as the types of functions.
You can inspect the kind of a type in GHCi with `:k`:

```hs
> :k Int
Int :: *

> :k Num
Num :: * -> Constraint

> :k Maybe
Maybe :: * -> *

> :k Data.Map.Map
Data.Map.Map :: * -> * -> *
```

The `*` in the kind annotations means "any Haskell type."

Say we make a type constructor like

```hs
data Foo t a = Foo {x :: t a} deriving (Show)
```

We see its kind is

```hs
> :k Foo
Foo :: (* -> *) -> * -> *
```

This means that the `t` type parameter needs to be something with kind `* -> *`:

```hs
> Foo (Just 1)
Foo {x = Just 1}
```