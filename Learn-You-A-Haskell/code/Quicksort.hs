quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (pivot : xs) = quicksort lessThan ++ [pivot] ++ quicksort greaterThan
  where
    lessThan = filter (<= pivot) xs
    greaterThan = filter (> pivot) xs
