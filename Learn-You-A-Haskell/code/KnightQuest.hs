import Control.Monad
import Numeric.Natural

type Coordinate = (Natural, Natural)

inbounds :: Coordinate -> Bool
inbounds (x, y) = x `between` (0, 7) && y `between` (0, 7)
  where k `between` (low, high) = low <= k && k <= high

-- | The set of spaces a knight can be on,
-- not taking into account moves off the board.
moveKnightUnbounded :: Coordinate -> [Coordinate]
moveKnightUnbounded (x, y) =
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
moveKnight :: Coordinate -> [Coordinate]
moveKnight currentSpace = do
  nextSpace <- moveKnightUnbounded currentSpace
  guard (inbounds nextSpace)
  return nextSpace

-- | The set of spaces a knight can be on after exactly `n` moves.
knightMoves :: Natural -> Coordinate -> [Coordinate]
knightMoves n = foldl (>=>) return (replicate (fromIntegral n) moveKnight)

-- | Whether a square is reachable by a knight in exactly `n` moves.
reachable :: Natural -> Coordinate -> Coordinate -> Bool
reachable n a b = b `elem` knightMoves n a