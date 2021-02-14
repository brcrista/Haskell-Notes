quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (pivot : xs) =
  quicksort [x | x <- xs, x <= pivot]
  ++ [pivot]
  ++ quicksort [x | x <- xs, x > pivot]