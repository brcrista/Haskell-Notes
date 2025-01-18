module Tests.List where

import List
import Test.Tasty
import Test.Tasty.HUnit
import Tests.Helpers

-- Functor laws:
-- fmap id = id
-- fmap (f . g) = fmap f . fmap g

emptyList :: Num a => List a
emptyList = End
singletonList = List 0 End
longList = List 2 (List 1 (List 0 End))

test_functorIdentity = caseGroup "Functor identity law"
  [
    fmap id emptyList @?= id emptyList,
    fmap id singletonList @?= id singletonList,
    fmap id longList @?= id longList
  ]

test_functorDistributive = caseGroup "Functor distributive law"
  [
    fmap (succ . (*2)) emptyList @?= (fmap succ . fmap (*2)) emptyList,
    fmap (succ . (*2)) singletonList @?= (fmap succ . fmap (*2)) singletonList,
    fmap (succ . (*2)) longList @?= (fmap succ . fmap (*2)) longList
  ]

test_foldr = caseGroup "foldr"
  [
    foldr (+) 0 emptyList @?= 0,
    foldr (+) 1 emptyList @?= 1,
    foldr (+) 10 emptyList @?= 10,
    foldr (+) 0 singletonList @?= 0,
    foldr (+) 1 singletonList @?= 1,
    foldr (+) 10 singletonList @?= 10,
    foldr (+) 0 longList @?= 3,
    foldr (+) 1 longList @?= 4,
    foldr (+) 10 longList @?= 13
  ]

test_foldl = caseGroup "foldl"
  [
    foldl (+) 0 emptyList @?= 0,
    foldl (+) 1 emptyList @?= 1,
    foldl (+) 10 emptyList @?= 10,
    foldl (+) 0 singletonList @?= 0,
    foldl (+) 1 singletonList @?= 1,
    foldl (+) 10 singletonList @?= 10,
    foldl (+) 0 longList @?= 3,
    foldl (+) 1 longList @?= 4,
    foldl (+) 10 longList @?= 13
  ]

test_sum = caseGroup "sum"
  [
    sum emptyList @?= 0,
    sum singletonList @?= 0,
    sum longList @?= 3
  ]

test_length = caseGroup "length"
  [
    length emptyList @?= 0,
    length singletonList @?= 1,
    length longList @?= 3
  ]
