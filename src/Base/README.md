# Base

This is a re-implementation of Haskell's base libraries for learning purposes.

## Dependency structure

1. `Core`
1. `Control`, `Data`
1. `System`
1. `Prelude`

## Testing

To load a GHCi prompt without standard Prelude, run

```bash
ghci -XNoImplicitPrelude
```