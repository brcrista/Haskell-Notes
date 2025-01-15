# Semigroup and Monoid

## Semigroup

### Definition

A **semigroup** is simply a type that has an associative binary operation:

```hs
class Semigroup a where
  (<>) :: a -> a -> a
```

where `<>` is subject to the associativity law:

```hs
x <> (y <> z) == (x <> y) <> z
```

### Instances

This is clearly very common: `[a]`, `Bool`, and `Num a`, and `Ord a` all have operations like this already. `Maybe a`, `Either a b`, `a -> b`, and tuple types also define `<>` to form semigroups. Generally this means mapping the `<>` operation on a semigroup type argument:

```hs
> Just "hello" <> Just "world"
Just "helloworld"
> Just "hello" <> Nothing
Just "hello"
> show <> show $ 100
"100100"
```

`Bool`, `Num a`, and `Ord a` have multiple operations that qualify for `<>`. For that reason, they have wrapper classes that define `Semigroup` for that operation:
* `Bool`: `And`, `Or`
* `Num a`: `Sum a`, `Product a`
* `Ord a`: `Min a`, `Max a`

## Monoid

### Definition

A **monoid** is a semigroup with an identity or "zero" element:

```hs
x <> zero == x
```

The `Monoid` typeclass extends `Semigroup` to require types to define this value, called `mempty`:

```hs
class Semigroup a => Monoid a where
  mempty :: a
```

`Monoid` also defines

```hs
-- `mappend` is a historical artifact from before `Semigroup` was introduced
-- and is now deprecated
a -> a -> a
mappend = (<>)

[a] -> a
mconcat = foldr (<>) mempty
```

Naturally, `Monoid` requires the identity laws:

```hs
-- Right identity
x <> mempty == x

-- Left identity
mempty <> x == x
```

### Instances

The semigroups listed above are all monoids as well. For `Min` and `Max`, the identity elements are `maxBound` and `minBound`, respectively.

`Data.List.NonEmpty` is an example of a type that is a semigroup but *not* a monoid.