# Learnings

I think Haskell is a beautifully designed language.
A handful of complementary choices lead naturally to the properties it is famous for.

All functions are pure:
- Everything is a function
- Declarative syntax
- Immutability
- `=` means "equal"
- Separation of I/O and application logic

Everything is a function:
- Laziness
- Polymorphic "values"
- Functions can be defined as other functions

Laziness:
- Separate data production from data consumption
- Currying by default
- Streaming I/O by default
- Can use normal lists / strings to represent data produced on-demand (ex. no need for a special "stream" type)

Currying by default:
- Partial application
- Functions are defined as in mathematics: n-ary functions (uncurried functions) operate on n-tuples
- Point-free style: defining functions in terms of other functions
- Combinators

Separation of I/O and application logic:
- Code is unit-testable by default
- Exceptions must be caught late and can't be abused for control flow
- Separation of computation and communication

Rich compile-time type syntax + type inference

## How should you define function composition?

There are two ways I've seen function composition defined in mathematics:

```
f*g(x) = f(g(x))
f*g(x) = g(f(x))
```

We should go with the definition that's more useful, and there are pros and cons to each.
Haskell defines it the first way.
With the concept of functors, function composition provides an additional benefit: functions are functors with `fmap = (.)`.

## Laziness

While a fully realized lazy-by-default language is an interesting experiment, and feels very nice for doing math, I'm not sure it's a good tool for software engineering.
Some drawbacks of laziness:
- Violates the principle of failing fast
- Makes it easy to consume lots of memory accidentally
- Makes it difficult to reason about memory usage

## Why I wouldn't use Haskell in production

I think Haskell is one of the best languages out there for solving math problems (along with Python and C++, depending on the application) and the best there is for formal work and testing out theorems.
However, there are a few reasons I wouldn't use it to build production software:

- Laziness-by-default can cause your code to consume lots of memory without you realizing it
- Relatively small standard library that is missing core functionality such as an HTTP client
- Base Prelude contains some unsafe or unperformant functions
- Libraries tend not to work together
- Libraries tend not to be maintained
- Slow prototyping
- Inflexibililty around I/O makes it hard to "just stick a trace in there"
- Lack of a good IDE and other tooling
- Not a language usually targeted by SDKs, have to write custom wrappers around REST APIs, for example
- Steep learning curve
- Slow compilation phase

Haskell is deeply influential to the functional programming discipline, but I would rather choose a more pragmatic language such as F# for building software.
