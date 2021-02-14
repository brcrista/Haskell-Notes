import Numeric.Natural

fibonacci :: [Natural]
fibonacci = 0 : 1 : zipWith (+) (tail fibonacci) fibonacci