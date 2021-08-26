module Bytestrings.Copy where

import           System.Environment             ( getArgs )
import qualified Data.ByteString.Lazy          as BL


main :: IO ()
main = do
  (f1 : f2 : _) <- getArgs
  copyFile f1 f2

copyFile :: String -> String -> IO ()
copyFile f1 f2 = do
  contents <- BL.readFile f1
  BL.appendFile f2 contents
  print "Done"
