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

test_functorDistributive = caseGroup "Functor distributive law"
  [
    fmap (succ . (*2)) emptyTree @?= (fmap succ . fmap (*2)) emptyTree,
    fmap (succ . (*2)) singletonTree @?= (fmap succ . fmap (*2)) singletonTree,
    fmap (succ . (*2)) leftTree @?= (fmap succ . fmap (*2)) leftTree,
    fmap (succ . (*2)) rightTree @?= (fmap succ . fmap (*2)) rightTree,
    fmap (succ . (*2)) balancedTree @?= (fmap succ . fmap (*2)) balancedTree
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

-- `length` returns the number of nodes in the tree
test_length = caseGroup "length"
  [
    length emptyTree @?= 0,
    length singletonTree @?= 1,
    length leftTree @?= 3,
    length rightTree @?= 3,
    length balancedTree @?= 3
  ]

test_height = caseGroup "height"
  [
    height emptyTree @?= 0,
    height singletonTree @?= 1,
    height leftTree @?= 3,
    height rightTree @?= 3,
    height balancedTree @?= 2
  ]

test_mirror = caseGroup "mirror"
  [
    mirror emptyTree @?= emptyTree,
    mirror singletonTree @?= singletonTree,
    mirror leftTree @?= rightTree,
    mirror rightTree @?= leftTree,
    mirror balancedTree @?= Node 1 (Node 2 Nil Nil) (Node 0 Nil Nil)
  ]

test_rotateLeft = caseGroup "rotateLeft"
  [
    rotateLeft emptyTree @?= emptyTree,
    rotateLeft singletonTree @?= singletonTree,
    rotateLeft leftTree @?= Node 1 singletonTree (Node 2 Nil Nil),
    rotateLeft rightTree @?= rightTree,
    rotateLeft balancedTree @?= Node 0 Nil (Node 1 Nil (Node 2 Nil Nil))
  ]

test_rotateRight = caseGroup "rotateRight"
  [
    rotateRight emptyTree @?= emptyTree,
    rotateRight singletonTree @?= singletonTree,
    rotateRight leftTree @?= leftTree,
    rotateRight rightTree @?= Node 1 (Node 2 Nil Nil) singletonTree,
    rotateRight balancedTree @?= leftTree
  ]