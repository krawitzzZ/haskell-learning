module Exceptions.Main where

import           System.Environment
import           System.IO.Error


main :: IO ()
main = do
  -- throws exception if no such file
  -- (fileName : _) <- getArgs
  -- contents       <- readFile fileName
  -- putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

  -- if file doesn't exists log some message
  -- (fileName : _) <- getArgs
  -- fileExists     <- doesFileExist fileName
  -- if fileExists
  --   then do
  --     contents <- readFile fileName
  --     putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"
  --   else do
  --     putStrLn "The file doesn't exist!"

  -- catch tries to execute IO action and calls handler if error occurred
  execute `catchIOError` errorHandler

execute :: IO ()
execute = do
  (fileName : _) <- getArgs
  contents       <- readFile fileName
  putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

errorHandler :: IOError -> IO ()
errorHandler e
  | isDoesNotExistError e = case ioeGetFileName e of
    Nothing ->
      putStrLn "The file doesn't exist! I don't even know the file name :("
    Just fileName -> putStrLn $ "The file " ++ fileName ++ " doesn't exist"
  | otherwise = ioError e
-- The predicates that act on IOError are:
-- isAlreadyExistsError
-- isDoesNotExistError
-- isAlreadyInUseError
-- isFullError
-- isEOFError
-- isIllegalOperation
-- isPermissionError
-- isUserError
-- list of error getters: https://downloads.haskell.org/~ghc/6.10.1/docs/html/libraries/base/System-IO-Error.html#3
