# Haskell

The file names here correspond to the modules in *Learn You a Haskell*: <http://learnyouahaskell.com>.

Other resources:
- [*A Gentle Introduction to Haskell*](https://www.haskell.org/tutorial/index.html)
- [`Prelude` reference on Hackage](https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html)
- [*Typeclassopedia*](https://wiki.haskell.org/Typeclassopedia)
- https://cabal.readthedocs.io/

## Learning path

- [ ] [*Learn You a Haskell for a Great Good!*](http://learnyouahaskell.com)
- [ ] [*A Gentle Introduction to Haskell*](https://www.haskell.org/tutorial/index.html)
- [ ] Re-implement `Prelude`
- [ ] Add HUnit tests
- [ ] Add QuickCheck tests
- [ ] Read the [`Prelude` source code](https://www.haskell.org/onlinereport/standard-prelude.html)
- [ ] Project Euler 1-20

## GHCi cheat sheet:

- `:q`: quit
- `:l`: load file
- `:t`: show type of expression
- `:k`: show kind of type constructor
- `:i`: show info for function, operator, or type

## Style guide

<https://wiki.haskell.org/Programming_guidelines>

### Naming
- Camel case for all names
- Types names are capitalized
- Function names are lowercase
- Defining operators should only be done by libraries

### Indentation
- Use spaces instead of tabs
- Format your code so you need to indent only one level at a time

```
-- Good
case foo of
    Foo -> "Foo"
    Bar -> "Bar"

-- Bad
case foo of Foo -> "Foo"
            Bar -> "Bar"
```

The "Bad" case can quickly end up with code too far to the right if clauses are nested.
You can also end up with irregular indentation (# of spaces not divisible by 4 or whatever you chose) and renamings can force you to re-indent everything.

### I/O
- Try to keep pure and impure (`do`) code in separate modules
- Don't write code where a different order of evaluation may lead to incorrect results.
    - Common example: reading and writing to the same file