
import System.Environment

main = do
  args <- getArgs
  let result = evaluateRPN $ head args
  print result

data Operator = Add | Subtract | Multiply | Divide

evaluateRPN :: String -> Int
evaluateRPN rpn = evaluateRPNRecursive parsedRPN []
  where
    parsedRPN = map parse (words rpn)

evaluateRPNRecursive :: [Either Int Operator] -> [Int] -> Int
evaluateRPNRecursive [] [x] = x
evaluateRPNRecursive [] _ = error "invalid expression"
evaluateRPNRecursive ((Left num) : rest) xs = evaluateRPNRecursive rest (num : xs)
evaluateRPNRecursive ((Right op) : rest) (x1 : x2 : xs) = evaluateRPNRecursive rest (dispatchOp op x2 x1 : xs)
evaluateRPNRecursive ((Right op) : rest) _ = error "invalid expression"

parse :: String -> Either Int Operator
parse "+" = Right Add
parse "-" = Right Subtract
parse "*" = Right Multiply
parse "/" = Right Divide
parse x = Left $ read x

dispatchOp :: Operator -> Int -> Int -> Int
dispatchOp Add = (+)
dispatchOp Subtract = (-)
dispatchOp Multiply = (*)
dispatchOp Divide = div