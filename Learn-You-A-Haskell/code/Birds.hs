-- Simulate a tightrope walker with birds landing on their pole.
-- If one side has more than 3 birds than the other side, the tightrope walker falls.
-- Example:
-- Just (0, 0) >>= landLeft 1 = Just (1, 0)
-- Just (0, 0) >>= landLeft 1 >>= landRight 5 = Nothing
-- Just (0, 0) >>= landLeft 2 >>= landRight 2 = Just (2, 2)
-- Just (0, 0) >>= landLeft 2 >> Nothing >>= landRight 2 = Nothing

type Birds = Int
type Pole = (Birds, Birds)

isBalanced :: Pole -> Bool
isBalanced (left, right) = abs (left - right) < 4

balance :: Pole -> Maybe Pole
balance pole
  | isBalanced pole = Just pole
  | otherwise       = Nothing

landLeft :: Birds -> Pole -> Maybe Pole
landLeft birds (left, right) = balance (left + birds, right)

landRight :: Birds -> Pole -> Maybe Pole
landRight birds (left, right) = balance (left, right + birds)
