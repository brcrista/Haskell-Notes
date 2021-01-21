# Haskell

Resources:
- [*Learn You a Haskell for a Great Good!*](http://learnyouahaskell.com)
- [*A Gentle Introduction to Haskell*](https://www.haskell.org/tutorial/index.html)
- [`Prelude` reference on Hackage](https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html)
- [*Typeclassopedia*](https://wiki.haskell.org/Typeclassopedia)
- <https://cabal.readthedocs.io>

## Learning path

- [ ] [*Learn You a Haskell for a Great Good!*](http://learnyouahaskell.com)
- [ ] [*A Gentle Introduction to Haskell*](https://www.haskell.org/tutorial/index.html)
- [ ] Re-implement `Prelude`
- [ ] Add HUnit tests
- [ ] Add QuickCheck tests
- [ ] Read the [`Prelude` source code](https://www.haskell.org/onlinereport/standard-prelude.html)
- [x] Falling water in Haskell
- [x] Equilibrium index in Haskell
- [ ] Project Euler 1-20

## Installation

The easiest way to get up and running with Haskell "IRL" is with [Stack](https://docs.haskellstack.org/en/stable/README/).
This will install GHC (the compiler), Cabal (the build system and package manager), and Stack (a Cabal wrapper for reproducible builds and isolated environments).

When creating a project, `stack new` will give you a template to work from.

### VSCode

- Install the [Haskell extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell)

## GHCi cheat sheet:

- `:q`: quit
- `:l`: load file
- `:t`: show type of expression
- `:k`: show kind of type
- `:i`: show info for function, operator, or type

## Style guide

<https://wiki.haskell.org/Programming_guidelines>

I've looked at a couple code formatters.
I've tried `ormulu` through the Haskell VS Code extension. It doesn't offer any sort of config, unfortunately.
I've tried `stylish-haskell` and played around with the config, but couldn't really get a result I like.

I mostly care about lining up `=` in `where` clauses, cases, and patterns, and lining up `::` in records.
I'm not too crazy about lining up import statements, as long as they're sorted.
I don't like putting `,` before comma-separated elements.
I get that it keeps things lined up but just doesn't look natural.
Every formatter seems to do that.
I also prefer Allman-style braces, but both formatters want to put the first element on the same line as the brace.

And I'm not being pragmatic here and just pick a format and sticking with it.
I'm fine doing that with JavaScript, but I'm coding in Haskell because I like the way it looks.

### Naming

- Camel case for all names
- Types names are capitalized
- Function names are lowercase
- Defining operators should only be done by libraries

### Indentation

- Use spaces instead of tabs
- Format your code so you need to indent only one level at a time

```hs
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
