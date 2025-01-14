module Tests.BinaryTree where

import BinaryTree
import Test.Tasty
import Test.Tasty.HUnit
import Tests.Helpers

-- Functor laws:
-- fmap id = id
-- fmap (f . g) = fmap f . fmap g

emptyTree :: Num a => BinaryTree a
emptyTree = Nil
singletonTree = Node 0 Nil Nil
leftTree = Node 2 (Node 1 singletonTree Nil) Nil
rightTree = Node 2 Nil (Node 1 Nil singletonTree)
balancedTree = Node 1 (Node 0 Nil Nil) (Node 2 Nil Nil)

test_functorIdentity = caseGroup "Functor identity law"
  [
    fmap id emptyTree @?= emptyTree,
    fmap id singletonTree @?= id singletonTree,
    fmap id leftTree @?= id leftTree,
    fmap id rightTree @?= id rightTree,
    fmap id balancedTree @?= id balancedTree
  ]

test_foldr = caseGroup "foldr"
  [
    foldr (+) 0 emptyTree @?= 0,
    foldr (+) 1 emptyTree @?= 1,
    foldr (+) 10 emptyTree @?= 10,
    foldr (+) 0 singletonTree @?= 0,
    foldr (+) 1 singletonTree @?= 1,
    foldr (+) 10 singletonTree @?= 10,
    foldr (+) 0 leftTree @?= 3,
    foldr (+) 1 leftTree @?= 4,
    foldr (+) 10 leftTree @?= 13,
    foldr (+) 0 rightTree @?= 3,
    foldr (+) 1 rightTree @?= 4,
    foldr (+) 10 rightTree @?= 13,
    foldr (+) 0 balancedTree @?= 3,
    foldr (+) 1 balancedTree @?= 4,
    foldr (+) 10 balancedTree @?= 13
  ]

test_foldl = caseGroup "foldl"
  [
    foldl (+) 0 emptyTree @?= 0,
    foldl (+) 1 emptyTree @?= 1,
    foldl (+) 10 emptyTree @?= 10,
    foldl (+) 0 singletonTree @?= 0,
    foldl (+) 1 singletonTree @?= 1,
    foldl (+) 10 singletonTree @?= 10,
    foldl (+) 0 leftTree @?= 3,
    foldl (+) 1 leftTree @?= 4,
    foldl (+) 10 leftTree @?= 13,
    foldl (+) 0 rightTree @?= 3,
    foldl (+) 1 rightTree @?= 4,
    foldl (+) 10 rightTree @?= 13,
    foldl (+) 0 balancedTree @?= 3,
    foldl (+) 1 balancedTree @?= 4,
    foldl (+) 10 balancedTree @?= 13
  ]

test_sum = caseGroup "sum"
  [
    sum emptyTree @?= 0,
    sum singletonTree @?= 0,
    sum leftTree @?= 3,
    sum rightTree @?= 3,
    sum balancedTree @?= 3
  ]