# Making Our Own Types and Typeclasses

## Algebraic data types

### Union types

We can define a new type with the `data` keyword:

```
data Bool = False | True
```

`False` and `True` here are called **data constructors**.
Data constructors can also take parameters:

```
> data Shape = Circle (Float, Float) Float | Rectangle (Float, Float) Float Float
> c = Circle (0, 0) 1
> :t c
c :: Shape
```

If a data constructor takes no parameters, it is said to be **nullary**.
Of course, data constructors are just functions:

```
> :t Shape
Circle :: (Float, Float) -> Float -> Shape
```

Any data constructor can be used in pattern matching:

```
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

```
module Shapes(
    Shape (..) -- export all
    Shape (Circle, Rectange) -- export all
    Shape (Cirlce) -- export Circle
    Shape -- export none, need to use some function to create instances
)
```

### Sum types

Sum types, also called **records**, give you a way to define a data constructor and a bunch of functions for operating on it.

So, we could define `Shape` as

```
data Shape =
    Circle { center :: (Float, Float), radius :: Float }
    | Rectangle { bottomLeftCorner :: (Float, Float), width :: Float, height :: Float }
```

The data constructors are called in the same way, but then we can also do:

```
> width $ Rectangle (0, 0) 5 3
5.0
```

The default implementation of `Show` will also use these names.

## Type constructors

The type name is actually a constructor too.
It's called a **type constructor**, and we can give it parameters:


```
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

```
data Color = Red | Green | Blue deriving (Show, Eq, Enum)
```

we can do:

```
> succ Red
Green

> Red == Red
True
```

Note that the implementation of `==` actually checks if the same data constructor was used for the two values as well as the actual fields.

## Defining a typeclass

We can define our own typeclass with the `class` keyword:

```
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

    x == y = not (x /= y)
    x /= y = not (x == y)
```

The two operators belonging to `Eq` are defined in terms of each other.
When we go to implement `Eq`, we only need to define one of these operators, and the other will automatically be implmented.

You implement a typeclass like this:

```
instance Eq Shape where
    -- Define `==`
```

To make a type constructor produce instances of a type class, do:

```
instance Eq m => Eq (Maybe m) where
    -- Define `==`
```

A typeclass can also be a subclass of another typeclass.
Just add a typeclass constraint as usual: `class Eq a => Num a where`.

If you poke around in GHCi with `:i`, you'll see that a whole lot of the built-in functions are actually defined in typeclasses.
Note that, unlike .NET, you can have non-user-defined types implement typeclasses.

## Type synonym

A **type synonym** is another name for a type.
The two names are considered equivalent for type checking.

```
type String = [Char]
```

Type synonyms can help make our code more self-documenting without the overhead of defining and using new types.