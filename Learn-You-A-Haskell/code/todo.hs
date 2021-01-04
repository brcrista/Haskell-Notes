import Data.List
import Data.Maybe (fromMaybe)
import System.Directory
import System.Environment
import System.IO

usage = error "Usage: todo COMMAND FILENAME [ARGS]..."

type Action = [String] -> IO ()

dispatch :: [(String, Action)]
dispatch =
  [ ("add", add),
    ("view", view),
    ("remove", remove),
    ("bump", bump)
  ]

lookupCommand command = fromMaybe usage (lookup command dispatch)

main = do
  (command : args) <- getArgs
  let action = lookupCommand command
  action args

add :: Action
add [filename, todoItem] = appendFile filename (todoItem ++ "\n")
add _ = usage

view :: Action
view [filename] = do
  contents <- readFile filename
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0 ..] todoTasks
  putStr $ unlines numberedTasks
view _ = usage

remove :: Action
remove [filename, numberString] = do
  cwd <- getCurrentDirectory
  handle <- openFile filename ReadMode
  (tempName, tempHandle) <- openTempFile cwd "temp"
  contents <- hGetContents handle
  let number = read numberString
      todoTasks = lines contents
      newTodoItems = delete (todoTasks !! number) todoTasks
  hPutStr tempHandle $ unlines newTodoItems
  hClose handle
  hClose tempHandle
  removeFile filename
  renameFile tempName filename
remove _ = usage

bump :: Action
bump [filename, numberString] = do
  cwd <- getCurrentDirectory
  handle <- openFile filename ReadMode
  (tempName, tempHandle) <- openTempFile cwd "temp"
  contents <- hGetContents handle
  let number = read numberString
      todoTasks = lines contents
      itemToBump = todoTasks !! number
      newTodoItems =  itemToBump : delete itemToBump todoTasks
  hPutStr tempHandle $ unlines newTodoItems
  hClose handle
  hClose tempHandle
  removeFile filename
  renameFile tempName filename
bump _ = usage