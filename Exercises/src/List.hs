module List
where

import Control.Applicative (Alternative, (<|>), empty)

data List a where
  End  :: List a
  List :: a -> List a -> List a
  deriving (Eq)

instance Show a => Show (List a) where
  show :: Show a => List a -> String
  show list = "[" ++ go list ++ "]"
    where
      go End = ""
      go (List x End) = show x
      go (List x xs)  = show x ++ "," ++ go xs

instance Semigroup (List a) where
  (<>) :: List a -> List a -> List a
  list1 <> End   = list1
  End   <> list2 = list2
  (List x xs) <> list2 = List x (xs <> list2)

instance Monoid (List a) where
  mempty :: List a
  mempty = End

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap _ End = End
  fmap f (List x xs) = List (f x) (fmap f xs)

instance Applicative List where
  pure x = List x End

  End <*> _   = End
  _   <*> End = End
  List f fs <*> xs = (f <$> xs) <> (fs <*> xs)

instance Monad List where
  (>>=) :: List a -> (a -> List b) -> List b
  End >>= _ = End
  List x xs >>= f = f x <> (xs >>= f)

instance Alternative List where
  empty :: List a
  empty = mempty

  (<|>) :: List a -> List a -> List a
  (<|>) = (<>)

instance Foldable List where
  foldr :: (a -> b -> b) -> b -> List a -> b
  foldr _ acc End = acc
  foldr f acc (List x xs) = f x (foldr f acc xs)

instance Traversable List where
  sequenceA :: Applicative f => List (f a) -> f (List a)
  sequenceA = foldr (liftA2 List) (pure End)

idx :: List a -> Int -> a
idx End _ = error "index too large"
idx (List x xs) n
  | n  < 0    = error "negative index"
  | n == 0    = x
  | otherwise = idx xs (n - 1)

reverse' :: List a -> List a
reverse' End = End
reverse' (List x xs) = reverse' xs <> List x End

range :: (Enum a, Ord a) => a -> a -> List a
range a b
  | a > b = End
  | otherwise = List a (range (succ a) b)