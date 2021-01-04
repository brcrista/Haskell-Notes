# Modules

A **module** in Haskell is a collection of:
- functions
- types
- typeclasses

The Haskell standard library consists of many modules.
Everything that is imported by default is in the `Prelude` module.

Modules are imported with `import <module name>` at the top of the file.
There are lots of varieties of this syntax to avoid clashes:

For example:

```hs
import Data.List
import Data.List (nub, sort)
import Data.List hiding (nub)
import qualified Data.List
import qualified Data.List as L
```

Some other common modules besides `Prelude`:

## `Data.Char`

## `Data.List`

```hs
\\
delete
elemIndices
elemIndex
find
findIndices
findIndex
foldl'
foldl1'
group
inits
insert
intercalate
intersect
intersperse
isInfixOf
isPrefixOf
isSuffixOf
lookup
nub
partition
sort
tails
transpose
union
zip4 -- etc.
zipWith4 -- etc.
```

Note: a lot of list functions exposed by `Prelude` are actually defined in `Data.List`!

## `Data.Map`

```hs
Map
elems
empty
insert
keys
filter
fromList
lookup
map
member
null
singleton
size
toList
```

## `Data.Set`

```hs
delete
difference
empty
filter
fromList
insert
intersection
map
member
null
singleton
size
toList
union
```

## Maybe

`Maybe` wraps a type to create a new type that may or may not have a value.
So, `Maybe` is an example of a **type constructor**, which is similar to generic types in C#.
(Note that all functions in Haskell are already "generic!")

You use it like

```hs
Just 1  -- Creates a `Num a => Maybe a` with a value
Nothing -- Creates a `Maybe a` with no value
```

Note that `Just 1` is still "generic!"
In C#, we have to close a generic type before we can create objects out of it.
In Haskell, there are no objects -- everything is just a function.
So `Just 1` is a function, and Haskell will figure out how that number is represented when it's used.

## Map

The `Data.Map` module contains the `Map` type constructor.
This has nothing to do with the `map` function and is just the normal map data structure.

```hs
> :t fromList [("a", 1)]
fromList [("a", 1)] :: Num a => Map String -> a
```

Maps in Haskell (and sets also) are implemented with trees, not hash tables.
Therefore, the key type for the map must derive from `Ord`.

**Note:** because `Data.Map` and `Data.Set` have functions that clash with `Prelude`, `Data.List` and each other, it's good practice to import them qualified:

```hs
import qualified Data.Map as Map
import qualified Data.Set as Set
```