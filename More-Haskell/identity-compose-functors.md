# The Identity and Compose Functors

I couldn't find this anywhere on the Internet, so I'm jotting down some examples here.

## Identity

```hs
> import Data.Functor.Identity
> fmap succ (Identity 1)
Identity 2
```

## Compose

```hs
> import Data.Functor.Compose
> let singleton x = [x]
> fmap succ (singleton (Just 1))
-- Type error
> fmap succ Compose (singleton (Just 1))
> Compose [Just 2]
```