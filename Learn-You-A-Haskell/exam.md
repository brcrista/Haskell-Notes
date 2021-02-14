# Exam

1. Implement the following functions from `Prelude`:

    ```hs
    flip :: (a -> b -> c) -> b -> a -> c
    repeat :: a -> [a]
    foldl :: (b -> a -> b) -> b -> [a] -> b
    -- Define `foldl` just for lists.
    -- Note that the actual definition of `foldl` uses `Foldable t => t a` instead of [a].
    ```
2. Implement the `(.)` the `($)` operators and give their type signatures.
3. Give the type signatures of the following functions:

    ```hs
    fmap
    (<*>)
    (>>=)
    ```
4. What is the difference between a typeclass and a type constructor? Give an example of each.
5. List the functor laws.
6. List the monad laws.
7. What is a **sum type**? What is a **product type**?
8. Write a function in Haskell that produces an infinite list of the Fibonacci numbers.
9. Implement quicksort in Haskell.
10. Write a function in Haskell that evaluates a tic-tac-toe board to see which player, if either, has one the game.