# Learnings

I think Haskell is a beautifully designed language.
A handful of complementary choices lead naturally to the properties it is famous for.

All functions are pure:
- Everything is a function
- Declarative syntax
- Immutability
- Separation of I/O and application logic

Everything is a function:
- Laziness
- Polymorphic "values"

Laziness:
- Separate data production from data consumption
- Currying by default
- Streaming I/O by default
- Can use normal lists / strings to represent data produced on-demand (ex. no need for a special "stream" type)

Currying by default:
- Point-free style
- Combinators

Separation of I/O and application logic:
- Code is unit-testable by default
- Exceptions must be caught late and can't be abused for control flow
- Separation of computation and communication

Rich compile-time type syntax + type inference