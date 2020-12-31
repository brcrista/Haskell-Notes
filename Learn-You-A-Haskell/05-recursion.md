# Recursion

This chapter is about thinking recursively and implements a bunch of Prelude's list functions as practice.

## Quicksort

The (not in-place) quicksort implementation in Haskell is often touted as a poster child for functional programming.
This is the version using `filter` shown in chapter 6:

```hs
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (pivot:xs) = (quicksort lessThan) ++ [pivot] ++ (quicksort greaterThan)
    where
        lessThan = filter (<= pivot) xs
        greaterThan = filter (> pivot) xs
```

## Infinite lists and recursion

Note the definitions of these functions for infinite lists:

```hs
repeat' :: a -> [a]
repeat' x = x : repeat' x

cycle' :: [a] -> [a]
cycle' [] = emptyListError
cycle' xs = xs ++ cycle' xs
```

The recursion being on the right is what allows us to evaluate the list without falling down an infinite call stack.

```hs
-- This doesn't work.
repeat' x = repeat' x ++ [x]
```