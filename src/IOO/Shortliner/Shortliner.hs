module IOO.Shortliner.Shortliner where


main :: IO ()
main = interact $ unlines . filter ((< 10) . length) . lines -- the shortest version
-- main = interact onlyShorts -- interact is a fancy func that does the same as code below
-- main = do
  -- contents <- getContents
  -- putStrLn $ onlyShorts contents

-- onlyShorts :: String -> String
-- onlyShorts s = unlines shorties
--   where shorties = filter (\l -> length l < 10) $ lines s
