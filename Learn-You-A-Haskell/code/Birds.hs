-- Simulate a tightrope walker with birds landing on their pole.
-- If one side has more than 3 birds than the other side, the tightrope walker falls.
-- Example:
-- return (0, 0) >>= landLeft 1 = Right (1, 0)
-- return (0, 0) >>= landLeft 1 >>= landRight 5 = Left "Fell with (1,5)"
-- return (0, 0) >>= landLeft 2 >>= landRight 2 = Right (2, 2)
-- return (0, 0) >>= landLeft 2 >> Left "Banana" >>= landRight 2 = Left "Banana"

type Birds = Int
type Pole = (Birds, Birds)

isBalanced :: Pole -> Bool
isBalanced (left, right) = abs (left - right) < 4

balance :: Pole -> Either String Pole
balance pole
  | isBalanced pole = Right pole
  | otherwise       = Left $ "Fell with " ++ show pole

landLeft :: Birds -> Pole -> Either String Pole
landLeft birds (left, right) = balance (left + birds, right)

landRight :: Birds -> Pole -> Either String Pole
landRight birds (left, right) = balance (left, right + birds)
