import Data.List
import Data.Maybe (fromMaybe)
import System.Directory
import System.Environment
import System.IO

usage = error "Usage: todo COMMAND FILENAME [ARGS]..."

dispatch :: [(String, [String] -> IO ())]
dispatch =
  [ ("add", add),
    ("view", view),
    ("remove", remove)
  ]

lookupCommand command = fromMaybe usage (lookup command dispatch)

main = do
  (command : args) <- getArgs
  let action = lookupCommand command
  action args

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
add _ = usage

view :: [String] -> IO ()
view [fileName] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0 ..] todoTasks
  putStr $ unlines numberedTasks
view _ = usage

remove :: [String] -> IO ()
remove [fileName, numberString] = do
  handle <- openFile fileName ReadMode
  cwd <- getCurrentDirectory
  (tempName, tempHandle) <- openTempFile cwd "temp"
  contents <- hGetContents handle
  let number = read numberString
      todoTasks = lines contents
      newTodoItems = delete (todoTasks !! number) todoTasks
  hPutStr tempHandle $ unlines newTodoItems
  hClose handle
  hClose tempHandle
  removeFile fileName
  renameFile tempName fileName
remove _ = usage