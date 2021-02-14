# Exam

**Time:** 50 minutes

You may use GHCi.

To get started, run

```bash
touch {01..03}.hs
touch {04..07}.md
touch {08..10}.hs
```

in this directory.
Put the answers in those files.

1. Implement the following functions from `Prelude`:

    ```hs
    flip :: (a -> b -> c) -> b -> a -> c
    repeat :: a -> [a]
    foldl :: (b -> a -> b) -> b -> [a] -> b
    -- Define `foldl` just for lists.
    -- Note that the actual definition of `foldl` uses `Foldable t => t a` instead of [a].
    ```
    *Hint:* Use `import Prelude()` to appease the compiler.
2. Implement the `(.)` the `($)` operators and give their type signatures. Don't worry about fixity declarations.
   *Hint:* Use `import Prelude()` to appease the compiler.
3. Give the type signatures (no implementations) of the following functions:

    ```hs
    fmap
    (<*>)
    (>>=)
    ```
    *Hint:* Use `import Prelude(Functor, Applicative, Monad)` to appease the compiler.
4. What is the difference between a typeclass and a type constructor? Give an example of each.
5. List the functor laws with their names and definitions.
6. List the monad laws with their names and definitions.
7. What is a **sum type**? What is a **product type**?
8. Write a function in Haskell that produces an infinite list of the Fibonacci numbers.
9. Implement quicksort in Haskell.
10. Write a function in Haskell that evaluates a tic-tac-toe board to see which player, if either, has one the game.