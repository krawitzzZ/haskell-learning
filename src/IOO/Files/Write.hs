module IOO.Files.Write where


import           Data.Char                      ( toUpper )


main :: IO ()
main = do

  contents <- readFile "./text.txt"
  writeFile "./capsedtext.txt" $ map toUpper contents
