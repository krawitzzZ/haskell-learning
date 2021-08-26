module IOO.Todos.Actions.View where

import           IOO.Todos.Utils                ( orderedTodos
                                                , getTodoFilePath
                                                )
import           System.IO                      ( hGetContents
                                                , withFile
                                                , IOMode(ReadMode)
                                                , Handle
                                                )


view :: [String] -> IO ()
view _ = do
  file <- getTodoFilePath
  withFile file ReadMode showContents

showContents :: Handle -> IO ()
showContents h = do
  contents <- hGetContents h
  putStrLn "TODO list:"
  putStrLn . unlines . orderedTodos . lines $ contents
