module IOO.Todos.Actions.Remove where

import           IOO.Todos.Utils                ( getTodoFilePath
                                                , getTodoFileDir
                                                , todoByNumber
                                                )
import           System.IO                      ( IOMode(ReadMode)
                                                , hGetContents
                                                , openTempFile
                                                , hPutStrLn
                                                , openFile
                                                , hClose
                                                )
import           Data.List                      ( (\\) )
import           System.Directory               ( renameFile
                                                , removeFile
                                                )


remove :: [String] -> IO ()
remove strNums = do
  putStrLn $ "Removing next TODOS: " ++ unwords strNums
  removeTodos (map read strNums :: [Int])

removeTodos :: [Int] -> IO ()
removeTodos nums = do
  filePath <- getTodoFilePath
  handle   <- openFile filePath ReadMode
  contents <- hGetContents handle

  let todos         = lines contents
      todosToRemove = map (`todoByNumber` todos) nums
      newTodos      = todos \\ todosToRemove

  todoFileDir            <- getTodoFileDir
  (tempName, tempHandle) <- openTempFile todoFileDir "temp"

  mapM_ (\t -> hPutStrLn tempHandle t) newTodos

  hClose handle
  hClose tempHandle
  removeFile filePath
  renameFile tempName filePath

  putStrLn "Done"
