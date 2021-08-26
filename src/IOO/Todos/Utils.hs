module IOO.Todos.Utils where

import           System.Directory               ( getCurrentDirectory )


todoFile :: String
todoFile = "todos.txt"

todoDir :: String
todoDir = "/src/IOO/Todos"

getTodoFileDir :: IO String
getTodoFileDir = do
  dir <- getCurrentDirectory
  return $ dir ++ todoDir

getTodoFilePath :: IO String
getTodoFilePath = do
  dir <- getCurrentDirectory
  return $ dir ++ todoDir ++ "/" ++ todoFile

orderedTodos :: [String] -> [String]
orderedTodos = zipWith (\n l -> show n ++ ": " ++ l) [(1 :: Integer) ..]

todoByNumber :: Int -> [String] -> String
todoByNumber num tds = tds !! (num - 1)
