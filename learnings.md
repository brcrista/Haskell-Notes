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