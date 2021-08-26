module IOO.Todos.Actions.Add where

import           IOO.Todos.Utils                ( getTodoFilePath
                                                , getTodoFileDir
                                                )
import           System.Directory               ( removeFile
                                                , renameFile
                                                )
import           System.IO                      ( hClose
                                                , hGetContents
                                                , openFile
                                                , hPutStrLn
                                                , openTempFile
                                                , IOMode(ReadMode)
                                                )


add :: [String] -> IO ()
add args = do
  putStrLn "Adding some TODOS.."
  putStrLn $ unlines args
  addTodos args

addTodos :: [String] -> IO ()
addTodos todosToAdd = do
  todoFile <- getTodoFilePath
  handle   <- openFile todoFile ReadMode
  contents <- hGetContents handle

  let newTodos = lines contents ++ todosToAdd

  todoFileDir            <- getTodoFileDir
  (tempName, tempHandle) <- openTempFile todoFileDir "temp"

  mapM_ (\t -> hPutStrLn tempHandle t) newTodos

  hClose handle
  hClose tempHandle
  removeFile todoFile
  renameFile tempName todoFile

  putStrLn "Done"
