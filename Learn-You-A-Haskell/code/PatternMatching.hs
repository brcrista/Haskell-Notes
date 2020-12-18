englishNumber :: Integral a => a -> String
englishNumber 0 = "zero"
englishNumber 1 = "one"
englishNumber 2 = "two"
englishNumber 3 = "three"
englishNumber 4 = "four"
englishNumber 5 = "five"
englishNumber n = "Not between 0 and 5"

englishNumber' :: Integral a => a -> String
englishNumber' n
  | n == 0 = "zero"
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | otherwise = "Not between 0 and 5"

englishNumber'' :: Integral a => a -> String
englishNumber'' n =
  case n of
    0 -> "zero"
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    _ -> "Not between 0 and 5"

factorial :: Integral a => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addPairs (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

head' [] = error "empty list"
head' (x : _) = x

-- Mass in kg, height in m
computeBmi mass height
  | bmi < 18.5 = "underweight"
  | bmi < 25.0 = "normal"
  | bmi < 30.0 = "overweight"
  | otherwise = "obese"
  where
    bmi = mass / (height ^ 2)

-- Find one of the zeroes of an equation in the form
-- ax^2 + bx + c = 0
quadratic a b c = ((- b) + sqrt determinant) / (2 * a)
  where
    determinant = (b ^ 2) - (4 * a * c)