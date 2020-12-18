# Recursion

This chapter is about thinking recursively and implements a bunch of Prelude's list functions as practice.

## Quicksort

The (not in-place) quicksort implementation in Haskell is often touted as a poster child for functional programming.
This is the version using `filter` shown in chapter 6:

```
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (pivot:xs) = (quicksort lessThan) ++ [pivot] ++ (quicksort greaterThan)
    where
        lessThan = filter (<= pivot) xs
        greaterThan = filter (> pivot) xs
```