add(x, y) = x + y

quadraticFormula(a, b, c) = do
  let
    discriminant = b ** 2 - 4 * a * c
    rhs = sqrt(discriminant) / (2 * a)
  return ((-b) + rhs, (-b) - rhs)