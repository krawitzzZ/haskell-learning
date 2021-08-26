module IOO.Files.Read where


main :: IO ()
main = do
--   handle   <- openFile "./text.txt" ReadMode
--   contents <- hGetContents handle
--   putStrLn contents
--   hClose handle

--   withFile
--     "./text.txt"
--     ReadMode
--     (\h -> do
--       contents <- hGetContents h
--       putStrLn contents
--     )

  contents <- readFile "./text.txt"
  putStrLn contents
