module IOO.Todos.Main where

import qualified IOO.Todos.Actions.Main        as Actions
import           System.Environment             ( getArgs )

main :: IO ()
main = do
  (cmd : args) <- getArgs
  if null cmd then putStrLn "you forgot command" else executeAction cmd args

executeAction :: String -> [String] -> IO ()
executeAction cmd = execute maybeAction
  where maybeAction = lookup cmd dispatch

execute :: Maybe ([String] -> IO ()) -> [String] -> IO ()
execute Nothing       _    = print "Unknown command"
execute (Just action) args = action args

dispatch :: [(String, [String] -> IO ())]
dispatch =
  [("add", Actions.add), ("view", Actions.view), ("remove", Actions.remove)]
