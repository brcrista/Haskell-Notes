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
singletonList = pure 0
longList = List 2 . List 1 . List 0 $ End

test_functorIdentity = caseGroup "functor identity law"
  [
    fmap id emptyList @?= id emptyList,
    fmap id singletonList @?= id singletonList,
    fmap id longList @?= id longList
  ]

test_functorDistributive = caseGroup "functor distributive law"
  [
    fmap (f . g) emptyList @?= (fmap f . fmap g) emptyList,
    fmap (f . g) singletonList @?= (fmap f . fmap g) singletonList,
    fmap (f . g) longList @?= (fmap f . fmap g) longList
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

test_idx = caseGroup "idx"
  [
    singletonList `idx` 0 @?= 0,
    longList `idx` 0 @?= 2,
    longList `idx` 1 @?= 1,
    longList `idx` 2 @?= 0
  ]

test_reverse' = caseGroup "reverse'"
  [
    reverse' emptyList @?= emptyList,
    reverse' singletonList @?= singletonList,
    reverse' longList @?= (List 0 . List 1 . List 2 $ End)
  ]

f :: Int -> Int
f = succ

g :: Int -> Int
g = (*2)

test_applicative = caseGroup "applicative"
  [
    (End <*> emptyList) @?= emptyList,
    (End <*> longList) @?= emptyList,
    ((List f . List g $ End) <*> emptyList) @?= emptyList,
    ((List f . List g $ End) <*> longList) @?= (List 3 . List 2 . List 1 . List 4 . List 2 . List 0 $ End)
  ]

test_applicativeIdentity = caseGroup "applicative identity law"
  [
    (pure id <*> emptyList) @?= emptyList,
    (pure id <*> singletonList) @?= singletonList,
    (pure id <*> longList) @?= longList
  ]

test_applicativeComposition = caseGroup "applicative composition law"
  [
    (pure (.) <*> pure f <*> pure g <*> emptyList) @?= pure f <*> (pure g <*> emptyList),
    (pure (.) <*> pure f <*> pure g <*> singletonList) @?= pure f <*> (pure g <*> singletonList),
    (pure (.) <*> pure f <*> pure g <*> longList) @?= pure f <*> (pure g <*> longList)
  ]

test_applicativeHomomorphism = caseGroup "applicative homomorphism law"
  [
    (pure f <*> pure 0) @?= (pure (f 0) :: List Int),
    (pure f <*> pure 1) @?= (pure (f 1) :: List Int)
  ]

test_applicativeInterchange = caseGroup "applicative interchange law"
  [
    (pure f <*> pure 1) @?= (pure ($ 1) <*> pure f :: List Int)
  ]