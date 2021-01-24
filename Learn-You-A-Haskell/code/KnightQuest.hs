import Control.Monad

inbounds (x, y) = x `between` (0, 7) && y `between` (0, 7)
  where k `between` (low, high) = low <= k && k <= high

-- | The set of spaces a knight can be on,
-- not taking into account moves off the board.
knightMoveUnbounded (x, y) =
  [
    (x - 2, y - 1),
    (x - 2, y + 1),
    (x + 2, y - 1),
    (x + 2, y + 1),
    (x - 1, y - 2),
    (x - 1, y + 2),
    (x + 1, y - 2),
    (x + 1, y + 2)
  ]

-- | The set of spaces a knight can be on after one move.
knightMove currentSpace = do
  nextSpace <- knightMoveUnbounded currentSpace
  guard (inbounds nextSpace)
  return nextSpace

-- | The set of spaces a knight can be on after exactly 3 moves.
knightMove3 currentSpace = knightMove currentSpace >>= knightMove >>= knightMove

-- | Whether a square is reachable by a knight in exactly 3 moves.
reachable3 :: (Num a1, Num a2, Ord a1, Ord a2) => (a1, a2) -> (a1, a2) -> Bool
reachable3 a b = b `elem` knightMove3 a