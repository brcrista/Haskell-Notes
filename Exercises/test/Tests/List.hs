module Tests.List where

import Control.Applicative
import Data.Functor.Compose
import Data.Functor.Identity
import List
import Test.Tasty.HUnit
import Tests.Helpers

emptyList :: Num a => List a
emptyList = End
singletonList = pure 0
longList = List 2 . List 1 . List 0 $ End

test_semigroupAssociativity = caseGroup "semigroup associativity law"
  [
    ((emptyList <> emptyList) <> emptyList) @?= (emptyList <> (emptyList <> emptyList)),
    ((singletonList <> singletonList) <> singletonList) @?= (singletonList <> (singletonList <> singletonList)),
    ((longList <> longList) <> longList) @?= (longList <> (longList <> longList)),
    ((emptyList <> singletonList) <> longList) @?= (emptyList <> (singletonList <> longList))
  ]

test_monoidRightIdentity = caseGroup "monoid right identity law"
  [
    emptyList <> mempty @?= emptyList,
    singletonList <> mempty @?= singletonList,
    longList <> mempty @?= longList
  ]

test_monoidLeftIdentity = caseGroup "monoid left identity law"
  [
    mempty <> emptyList @?= emptyList,
    mempty <> singletonList @?= singletonList,
    mempty <> longList @?= longList
  ]

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
fM = range 0

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

test_monadAssociativity = caseGroup "monad associativity law"
  [
    (emptyList >>= (\x -> fM x >>= gM)) @?= ((emptyList >>= fM) >>= gM),
    (singletonList >>= (\x -> fM x >>= gM)) @?= ((singletonList >>= fM) >>= gM),
    (longList >>= (\x -> fM x >>= gM)) @?= ((longList >>= fM) >>= gM),
    (emptyList >>= (\x -> gM x >>= fM)) @?= ((emptyList >>= gM) >>= fM),
    (singletonList >>= (\x -> gM x >>= fM)) @?= ((singletonList >>= gM) >>= fM),
    (longList >>= (\x -> gM x >>= fM)) @?= ((longList >>= gM) >>= fM)
  ]

test_alternativeAssociativity = caseGroup "alternative associativity law"
  [
    ((emptyList <|> emptyList) <|> emptyList) @?= (emptyList <|> (emptyList <|> emptyList)),
    ((singletonList <|> singletonList) <|> singletonList) @?= (singletonList <|> (singletonList <|> singletonList)),
    ((longList <|> longList) <|> longList) @?= (longList <|> (longList <|> longList)),
    ((emptyList <|> singletonList) <|> longList) @?= (emptyList <|> (singletonList <|> longList))
  ]

test_alternativeRightIdentity = caseGroup "alternative right identity law"
  [
    emptyList <|> empty @?= emptyList,
    singletonList <|> empty @?= singletonList,
    longList <|> empty @?= longList
  ]

test_alternativeLeftIdentity = caseGroup "alternative left identity law"
  [
    empty <|> emptyList @?= emptyList,
    empty <|> singletonList @?= singletonList,
    empty <|> longList @?= longList
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

emptyListA :: List (Maybe Int)
emptyListA = End
longList1A = List (Just 2) . List (Just 1) . List (Just 0) $ End
longList2A = List (Just 2) . List Nothing . List (Just 0) $ End
longList3A = List Nothing . List (Just 1) . List (Just 0) $ End
longList4A = List (Just 2) . List (Just 1) . List Nothing $ End

-- An applicative transformation:
-- https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Traversable.html
tA :: Maybe a -> [a]
tA Nothing = []
tA (Just x) = pure x

test_sequenceA = caseGroup "sequenceA"
  [
    sequenceA emptyListA @?= Just End,
    sequenceA longList1A @?= Just longList,
    sequenceA longList2A @?= Nothing,
    sequenceA longList3A @?= Nothing,
    sequenceA longList4A @?= Nothing
  ]

test_sequenceANaturality = caseGroup "sequenceA naturality law"
  [
    (tA . sequenceA $ emptyListA) @?= (sequenceA . fmap tA $ emptyListA),
    (tA . sequenceA $ longList1A) @?= (sequenceA . fmap tA $ longList1A),
    (tA . sequenceA $ longList2A) @?= (sequenceA . fmap tA $ longList2A),
    (tA . sequenceA $ longList3A) @?= (sequenceA . fmap tA $ longList3A),
    (tA . sequenceA $ longList4A) @?= (sequenceA . fmap tA $ longList4A)
  ]

test_sequenceAIdentity = caseGroup "sequenceA identity law"
  [
    (sequenceA . fmap Identity $ emptyList) @?= Identity emptyList,
    (sequenceA . fmap Identity $ singletonList) @?= Identity singletonList,
    (sequenceA . fmap Identity $ longList) @?= Identity longList
  ]

emptyListC :: List (Maybe [Int])
emptyListC = End
longListC = List (Just [1]) . List Nothing $ End

test_sequenceACompose = caseGroup "sequenceA compose law"
  [
    (sequenceA . fmap Compose $ emptyListC) @?= (Compose . fmap sequenceA . sequenceA) emptyListC,
    (sequenceA . fmap Compose $ longListC) @?= (Compose . fmap sequenceA . sequenceA) longListC
  ]