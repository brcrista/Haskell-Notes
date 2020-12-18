class Truthy a where
  truthy :: a -> Bool

instance Truthy Bool where
  truthy = id

instance Truthy Int where
  truthy = (/= 0)

instance Truthy Integer where
  truthy = (/= 0)

instance Truthy [a] where
  truthy = not . null

instance Truthy a => Truthy (Maybe a) where
  truthy (Just x) = truthy x
  truthy Nothing  = False
