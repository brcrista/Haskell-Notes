# Lenses

The **lens** pattern provides a functional approach to getters and setters for nested data.

## Motivation

Consider the `Shape` type from *Learn You a Haskell*:

```hs
data Shape =
  Circle { center :: (Float, Float), radius :: Float }
  | Rectangle { bottomLeftCorner :: (Float, Float), width :: Float, height :: Float }
  deriving Show
```

If you have a `Circle`, you can access the inner data like:

```hs
> let circle = Circle (1, -1) 10
> center circle
(1.0,-1.0)
```

If you want to traverse the nested structure, you can use function composition:

```hs
> fst . center $ circle
1.0
```

However, consider trying to move the circle left 1 on the x-axis:


```hs
> let moveLeftX (x, y) = (pred x, y)
> Circle (moveLeftX $ center circle) (radius circle)
Circle {center = (0.0,-1.0), radius = 10.0}
```

We have to repeat all of the data that isn't changed -- the y-coordinate of the center and the circle's radius.
Of course, in a data type with lots of fields, this would be a problem.

Lenses simplify this:

```hs
-- Add `makeLenses ''Shape`
> center . _1 %~ pred $ circle
Circle {_center = (0.0,-1.0), _radius = 10.0}
```

Note that lenses compose right-to-left instead of left-to-right as functions normally do.
That is, the leftmost lens in a series of compositions is the first to be evaluated.

## How to Use Lenses

The `lens` package and its `Control.Lens` module provide the canonical implementation of lenses.
`lens` isn't part of `base`, but it ships with Stack.

There are three core combinators in `Control.Lens`:
1. `view` or `^.` takes a lens and gets a data field
1. `set` or `.~` takes a lens and a value replaces a data field
1. `over` or `%~` takes a lens and a function and maps it over a data field

Intuitively, a lens is a function that "focuses" on a data field for either reading or writing.
Lenses are composable for traversing nested fields.

There are some built-in lenses, like `_1`, `_2`, etc. for tuples or `ix` for lists. With the `TemplateHaskell` language extension, you can use `makeLenses` to generate lenses for a data type.

### Examples

A lens expression consists of:
1. Some data
1. A lens
1. A combinator
1. (For writing) a value or a function

```hs
> set (ix 3) 22 [1..5]
[1,2,3,22,5]

> set _2 22 (0, 1)
(0,22)

> _2 .~ 22 $ (0, 1)
(0,22)

> [1..10] ^? ix 6
Just 7

> over (mapped . _1) succ [(1, 2), (3, 4)]
[(2,2),(4,4)]

> mapped . _1 %~ succ $ [(1, 2), (3, 4)]
[(2,2),(4,4)]

> over (ix 0 . _1) succ [(1, 2), (3, 4)]
[(2,2),(3,4)]

> ix 0 . _1 %~ succ $ [(1, 2), (3, 4)]
[(2,2),(3,4)]
```

## Exercises

From <https://www.williamyaoh.com/posts/2019-04-25-lens-exercises.html>.
See `src/LensExercises.hs`.

### I

```hs
> user1 ^. name
"qiao.yifan"
> user1 ^. metadata.numLogins
20
> user1 & metadata.numLogins .~ 0
User
  { _name = "qiao.yifan"
  , _userid = 103
  , _metadata = UserInfo
    { _numLogins = 0
    , _associatedIPs =
      [ "52.39.193.61"
      , "52.39.193.75"
      ]
    }
  }
> user1 & metadata.associatedIPs %~ ("192.168.0.2" :)
User
  { _name = "qiao.yifan"
  , _userid = 103
  , _metadata = UserInfo
    { _numLogins = 20
    , _associatedIPs =
      [ "192.168.0.2"
      , "52.39.193.61"
      , "52.39.193.75"
      ]
    }
  }
> metadata.numLogins %~ (+ 1) $ user1
User
  { _name = "qiao.yifan"
  , _userid = 103
  , _metadata = UserInfo
    { _numLogins = 21
    , _associatedIPs =
      [ "52.39.193.61"
      , "52.39.193.75"
      ]
    }
  }
```

### II

| Compiles? | Code | Correction |
|--|
| ❌ | `user1 & email .~ "qyifan@xingxin.com"` | `user1 & name .~ "qyifan@xingxin.com"` |
| ✅ | `user1 & metadata .~ (UserInfo 17 [])` | |
| ✅ | `userid .~ -1 $ user1` | |
| ❌ | `metadata.associatedIPs .~ [ "50.193.0.23" ] & user1` | `user1 & metadata.associatedIPs .~ [ "50.193.0.23" ]` |
| ❌ | `user1 ^. numLogins.metadata` | `user1 ^. metadata.numLogins` |


### III

```hs
-- Get the associated IP addresses.
user1 ^. metadata.associatedIPs
-- Update the user so that the associated IP addresses are in reverse order.
user1 & metadata.associatedIPs %~ reverse
-- Update the user so that each word in the name is capitalized.
user1 & name .~ "Qiao.Yifan"
-- Set the number of logins to 1.
user1 & metadata.numLogins .~ 1
-- Remove all associated IP addresses except the first.
user1 & metadata.associatedIPs %~ init
```

### IV

```hs
infixr 4 .~
(.~) :: ((a -> Identity b) -> s -> Identity t) -> b -> s -> t
lens .~ newValue = runIdentity . lens (const $ Identity newValue) s

infixr 4 %~
(%~) :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
lens %~ f = runIdentity . lens (Identity . f) s

infixl 8 ^.
(^.) :: s -> ((a -> Const a b) -> s -> Const a t) -> a
s ^. lens = getConst $ lens Const s

name' :: Functor f => (Text -> f Text) -> User -> f User
name' fn user = fn (_name user) $> user
```

Function composition is defined as

```hs
(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f $ g x
```

And composed lenses have the type

```hs
lens1 :: Functor f => (a -> f a) -> (b -> f b)
lens2 :: Functor f => (b -> f b) -> (c -> f c)

lens2 . lens1 :: (a -> f a) -> c -> f c
```

The composition of two unary functions takes a value of type `a` as its parameter.
The composition of two lenses, when used with a lens combinator, takes a value of type `c`.

Internally, this inversion happens because the right-hand lens returns a function which is forwarded as the first argument of the left-hand lens and the getter wrapped by the lens is passed to it.

For example:

```hs
metadata :: Functor f => (UserInfo -> f UserInfo) -> User -> f User
metadata fn s = fn (_metadata s) $> s

numLogins :: Functor f => (Int -> f Int) -> UserInfo -> f UserInfo
numLogins fn s = fn (_numLogins s) $> s

metadata . numLogins
= \fn -> metadata $ numLogins fn
= \fn -> metadata $ \s -> fn (_numLogins s) $> s
= \fn -> \s' -> (\s -> fn (_numLogins s) $> s) (_metadata s') $> s'
```

## References
- <https://github.com/ekmett/lens/wiki>
- <https://www.williamyaoh.com/posts/2019-04-25-lens-exercises.html>