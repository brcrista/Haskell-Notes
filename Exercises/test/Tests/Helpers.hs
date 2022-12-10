module Tests.Helpers where

import Test.Tasty
import Test.Tasty.HUnit

-- | A group of test cases that labels each of its cases 1, 2, 3, etc.
caseGroup :: TestName -> [Assertion] -> TestTree
caseGroup title = testGroup title . zipWith testCase (show <$> [1 ..])