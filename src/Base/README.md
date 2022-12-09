# Base

This is a re-implementation of Haskell's base libraries for learning purposes.

## Dependency structure

1. `Core`
1. `Control`, `Data`
1. `System`
1. `Prelude`

## Testing

Navigate to `src/` (the directory above this one) and run `ghci`.

At the `ghci` prompt, run:

```
:set -XNoImplicitPrelude
:m -Prelude
:l Base/Prelude.hs
```