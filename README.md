# Haskell

## Resources

### References

- [Hackage: base](https://hackage.haskell.org/package/base-4.14.0.0)
- [Prelude source code](https://www.haskell.org/onlinereport/standard-prelude.html)
- [Numeric types quick reference](https://martingalemeasure.wordpress.com/2014/07/07/haskell-numeric-types-quick-reference/)

### Ecosystem

- [Hoogle](https://hoogle.haskell.org/)
- [QuickCheck](http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html)
- [QuickCheck tutorial](https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html)
- [GHC User's Guide](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/)
- [Cabal](https://cabal.readthedocs.io)
- [Stack](https://docs.haskellstack.org/en/stable/README/)
- [Haddock](https://haskell-haddock.readthedocs.io/en/latest/index.html)
- [pandoc](https://github.com/jgm/pandoc)

## Learning path

- [x] [*Learn You a Haskell for a Great Good!*](http://learnyouahaskell.com)
- [x] [*A Gentle Introduction to Haskell*](https://www.haskell.org/tutorial/index.html)
- [x] [Exam](./Learn-You-A-Haskell/exam/README.md)
- [x] [Haskell: The Bad Parts (Part 1)](https://www.snoyman.com/blog/2020/10/haskell-bad-parts-1/)
- [x] [Haskell: The Bad Parts (Part 2)](https://www.snoyman.com/blog/2020/11/haskell-bad-parts-2/)
- [x] [Tail recursion](https://wiki.haskell.org/Tail_recursion)
- [x] [All About Strictness](https://www.fpcomplete.com/haskell/tutorial/all-about-strictness/)
- [x] [Fixing foldl](https://www.well-typed.com/blog/2014/04/fixing-foldl/)
- [x] `Data.Foldable` and `Data.Traversable`
- [x] `Data.Function`
- [x] `Debug.Trace`
- [x] `Data.Text`
- [x] Semigroups
- [x] `System.FilePath`
- [x] [*A Guide to GHC's Extensions*](https://limperg.de/ghc-extensions/)
- [x] JSON (`aeson`)
- [x] Sending HTTP requests (`http-client` and `wreq`)
- [x] Calling a REST API
- [x] [`Data.Time`](https://www.williamyaoh.com/posts/2019-09-16-time-cheatsheet.html)
- [ ] [Regexes](https://www.williamyaoh.com/posts/2019-04-11-cheatsheet-to-regexes-in-haskell.html)
- [ ] Arrows
- [ ] [Lenses](https://www.williamyaoh.com/posts/2019-04-25-lens-exercises.html)
- [ ] `async`
- [ ] [*Real World Haskell*: Chapter 24](http://book.realworldhaskell.org/read/concurrent-and-multicore-programming.html)
- [x] [Bartosz Milewski's *Parallel and Concurrent Haskell* lectures](https://www.youtube.com/watch?v=N6sOMGYsvFA&list=PLbgaMIhjbmEm_51-HWv9BQUXcmHYtl4sw)
- [x] `Control.Concurrent`

### Projects

- [ ] Add QuickCheck tests to `Identities.hs`
- [x] Project Euler 1-20
- [x] Read the [`Prelude` source code](https://www.haskell.org/onlinereport/standard-prelude.html)
- [x] Falling Water in Haskell
- [x] Equilibrium Index in Haskell
- [x] Spiral Matrix in Haskell
- [ ] BST

## Installation

The easiest way to get up and running with Haskell is with [Stack](https://docs.haskellstack.org/en/stable/tutorial/).
This will install GHC (the compiler), Cabal (the build system and package manager), and Stack (a Cabal wrapper for reproducible builds and isolated environments).

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

- Don't write code where a different order of evaluation may lead to incorrect results.
    - Common example: reading and writing to the same file
