# Zippers

Since functions in Haskell are referentially transparent, manipulating data structures can be tricky.
For one, you can't just change a single element inside of a data structure.
You have to rebuild the whole data structure with the element replaced.
Moreover, traversing a data structure through pattern matching is often uni-directional.
For example, when traversing a list, you can only move forwards through the list, not backwards.
This is in contrast to object-oriented languages, where you can just hold a reference to the previous element in the list.

The **zipper pattern** is a technique to store context while manipulating a data structure.
For example, consider traversing a list:

```hs
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs
```

What if you need to move backwards in the list?
You can store the previous elements in a second list.
Then the **zipper** is the pair of previous elements and upcoming elements:

```hs
-- Note: you need to `reverse` the left elements when getting them back out
newtype ListZipper a = ListZipper { focus :: ([a], [a]) } deriving Show

start :: [a] -> ListZipper a
start xs = ListZipper ([], xs)

next :: ListZipper a -> ListZipper a
next (ListZipper (_, [])) = error "end of list"
next (ListZipper (xs, y : ys)) = ListZipper (y : xs, ys)

prev :: ListZipper a -> ListZipper a
prev (ListZipper ([], _)) = error "beginning of list"
prev (ListZipper (x : xs, ys)) = ListZipper (xs, x : ys)
```

As you traverse the list, element move from the "ahead" list into the "behind" list:

```hs
import Data.Function

> start [1..10] & next & next
ListZipper {focus = ([2,1],[3,4,5,6,7,8,9,10])}
```

As you can see, zippers are so named because it's like a zipper moving back and forth across the list.
It divides the list in two and elements slide back and forth as the "zipper" moves.
It has nothing to do with the `zip` family of functions, which use the same metaphor for a different concept.

Once we have a zipper, it becomes very easy to move around in the list and manipulate it.
We can do things like replace the current element, insert a sublist, or take the left or right part of the list and do something with it.

While immutability can be a pain in that we have to define zippers to manipulate data structures, it also means we never have to make copies of anything.