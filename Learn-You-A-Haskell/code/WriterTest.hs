import Control.Monad.Writer

addOne :: Int -> Writer String Int
addOne n = do
    tell $ show n ++ " + 1"
    return (n + 1)