-- This is a pretty painful memoization example I want to revisit and improve upon.
-- See https://wiki.haskell.org/Memoization
import qualified Data.Map

-- Build up a memo table to the nth Fibonacci number.
fibMemo :: Integral a => Int -> Data.Map.Map Int a -> Data.Map.Map Int a
fibMemo 0 _ = Data.Map.fromList [(0, 1)]
fibMemo 1 _ = Data.Map.fromList [(0, 1), (1, 1)]
fibMemo n memo =
  case Data.Map.lookup n memo of
    Just _ -> memo
    Nothing -> Data.Map.insert n nth prev
  where
    prevprev = fibMemo (n - 2) memo
    prev = fibMemo (n - 1) prevprev
    nth = fibLookup (n - 1) prev + fibLookup (n - 2) prev

fibLookup :: Integral a => Int -> Data.Map.Map Int a -> a
fibLookup n memo =
  case Data.Map.lookup n memo of
    Just result -> result
    Nothing -> error $ show n

fib n = fibLookup n $ fibMemo n Data.Map.empty