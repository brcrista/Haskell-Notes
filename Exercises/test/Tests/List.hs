module Tests.List where

import List
import Test.Tasty
import Test.Tasty.HUnit
import Tests.Helpers

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
    fmap (fA . gA) emptyList @?= (fmap fA . fmap gA) emptyList,
    fmap (fA . gA) singletonList @?= (fmap fA . fmap gA) singletonList,
    fmap (fA . gA) longList @?= (fmap fA . fmap gA) longList
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

fA :: Int -> Int
fA = succ

gA :: Int -> Int
gA = (*2)

test_applicative = caseGroup "applicative"
  [
    (End <*> emptyList) @?= emptyList,
    (End <*> longList) @?= emptyList,
    ((List fA . List gA $ End) <*> emptyList) @?= emptyList,
    ((List fA . List gA $ End) <*> longList) @?= (List 3 . List 2 . List 1 . List 4 . List 2 . List 0 $ End)
  ]

test_applicativeIdentity = caseGroup "applicative identity law"
  [
    (pure id <*> emptyList) @?= emptyList,
    (pure id <*> singletonList) @?= singletonList,
    (pure id <*> longList) @?= longList
  ]

test_applicativeComposition = caseGroup "applicative composition law"
  [
    (pure (.) <*> pure fA <*> pure gA <*> emptyList) @?= pure fA <*> (pure gA <*> emptyList),
    (pure (.) <*> pure fA <*> pure gA <*> singletonList) @?= pure fA <*> (pure gA <*> singletonList),
    (pure (.) <*> pure fA <*> pure gA <*> longList) @?= pure fA <*> (pure gA <*> longList)
  ]

test_applicativeHomomorphism = caseGroup "applicative homomorphism law"
  [
    (pure fA <*> pure 0) @?= (pure (fA 0) :: List Int),
    (pure fA <*> pure 1) @?= (pure (fA 1) :: List Int)
  ]

test_applicativeInterchange = caseGroup "applicative interchange law"
  [
    (pure fA <*> pure 1) @?= (pure ($ 1) <*> pure fA :: List Int)
  ]

fM :: Int -> List Int
fM x = range 0 x

gM :: Int -> List Int
gM x
  | x <= 0 = End
  | otherwise = List 0 (gM (pred x))

test_monadLeftIdenity = caseGroup "monad left identity law"
  [
    (return 0 >>= fM) @?= fM 0,
    (return 1 >>= fM) @?= fM 1,
    (return 0 >>= gM) @?= gM 0,
    (return 1 >>= gM) @?= gM 1
  ]

test_monadRightIdenity = caseGroup "monad right identity law"
  [
    (emptyList >>= return) @?= emptyList,
    (singletonList >>= return) @?= singletonList,
    (longList >>= return) @?= longList
  ]

-- m >>= (\x -> k x >>= h) = (m >>= k) >>= h
test_monadAssociativity = caseGroup "monad associativity law"
  [
    (emptyList >>= (\x -> fM x >>= gM)) @?= ((emptyList >>= fM) >>= gM),
    (singletonList >>= (\x -> fM x >>= gM)) @?= ((singletonList >>= fM) >>= gM),
    (longList >>= (\x -> fM x >>= gM)) @?= ((longList >>= fM) >>= gM),
    (emptyList >>= (\x -> gM x >>= fM)) @?= ((emptyList >>= gM) >>= fM),
    (singletonList >>= (\x -> gM x >>= fM)) @?= ((singletonList >>= gM) >>= fM),
    (longList >>= (\x -> gM x >>= fM)) @?= ((longList >>= gM) >>= fM)
  ]