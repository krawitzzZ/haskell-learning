module IOO.Files.Args where

import           System.Environment


main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  putStrLn "args:"
  mapM_ print args
  putStrLn "name:"
  print name
