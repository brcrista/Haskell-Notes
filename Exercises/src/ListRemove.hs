module ListRemove (
    remove
) where

remove :: Int -> [a] -> [a]
remove _ [] = []
remove n (x:xs) = removeRecursive n x xs

removeRecursive :: Int -> a -> [a] -> [a]
removeRecursive _ _ [] = []
removeRecursive 0 _ xs = xs
removeRecursive n next (x:xs) = next : (removeRecursive (n - 1) x xs)
