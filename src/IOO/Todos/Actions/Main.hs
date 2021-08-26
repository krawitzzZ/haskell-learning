module IOO.Todos.Actions.Main where

import qualified IOO.Todos.Actions.Add         as Add
import qualified IOO.Todos.Actions.View        as View
import qualified IOO.Todos.Actions.Remove      as Remove

add :: [String] -> IO ()
add = Add.add

view :: [String] -> IO ()
view = View.view

remove :: [String] -> IO ()
remove = Remove.remove
