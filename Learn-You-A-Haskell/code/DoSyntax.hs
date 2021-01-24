nonMonadic :: String
nonMonadic =
  let
    x = 3
    y = "!"
  in show x ++ y

monadicWithLambdas :: Maybe String
monadicWithLambdas =
  Just 3   >>= (\x ->
  Just "!" >>= (\y ->
  Just $ show x ++ y))

monadicWithDo :: Maybe String
monadicWithDo = do
  x <- Just 3
  y <- Just "!"
  Just $ show x ++ y