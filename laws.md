# Typeclass laws

## `Eq`

```hs
-- 1. Reflexivity
x == x = True

-- 2. Symmetry
x == y = y == x

-- 3. Transitivity
x == y && y == z ==> x == z

-- 4. Substitutivity
x == y ==> f x == f y

-- 5. Negation
x /= y == not $ x == y
```

## `Functor`

```hs
-- Identity law
fmap id = id

-- Distributive property over composition
fmap (f . g) = fmap f . fmap g
```

## `Applicative`

```hs
-- 1. Functor reduction law
pure f <*> x = f <$> x

-- 2. Identity law
pure id <*> x = x

-- 3. Homomorphism law
pure f <*> pure x = pure (f x)

-- 4. Composition law
pure (.) <*> x <*> y <*> z = x <*> (y <*> z)

-- 5. Interchange law
x <*> pure y = pure ($ y) <*> x
```

## `Monad`

```hs
-- 1. Left identity
return x >>= f = f x

-- 2. Right identity
m >>= return = m

-- 3. Associativity
(m >>= f) >>= g = m >>= (\x -> f x >>= g)
```

or

```hs
-- 1. Left identity
f <=< return = f

-- 2. Right identity
return <=< f = f

-- 3. Associativity
(f <=< g) <=< h = f <=< (g <=< h)
```