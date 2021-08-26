module IOO.Files.Todo where

import           System.IO
import           System.Directory
import           Data.List                      ( delete )


main :: IO ()
main = do
  handle                 <- openFile "./todos.txt" ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents               <- hGetContents handle
  let todos = lines contents
      numberedTodos =
        zipWith (\n l -> show n ++ ": " ++ l) [(1 :: Integer) ..] todos
  putStrLn "Here are your todos:"
  putStrLn $ unlines numberedTodos
  putStrLn "\n"
  putStrLn "Which number you'd like to delete?"
  numberStr <- getLine
  putStrLn "\n"
  let number   = read numberStr - 1
      newTodos = delete (todos !! number) todos
  hPutStrLn tempHandle $ unlines newTodos
  hClose handle
  hClose tempHandle
  removeFile "./todos.txt"
  renameFile tempName "todos.txt"
