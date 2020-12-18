class Shape a where
  area :: a -> Float

data Circle = Circle
  { center :: (Float, Float),
    radius :: Float
  }
  deriving (Show)

instance Shape Circle where
  area (Circle _ r) = pi * r ^ 2

data Rectangle = Rectangle
  { bottomLeftCorner :: (Float, Float),
    width :: Float,
    height :: Float
  }
  deriving (Show)

instance Shape Rectangle where
  area (Rectangle _ w h) = abs $ w * h
