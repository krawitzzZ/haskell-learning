module IOO.Capslocker.Capslocker where


import           Data.Char                      ( toUpper )


main :: IO ()
main = do
  contents <- getContents
  putStrLn $ map toUpper contents
