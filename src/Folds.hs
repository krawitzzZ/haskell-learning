module Folds where

import           Data.Time

data DatabaseItem = DbString String | DbNumber Integer | DbDate UTCTime deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello world!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  , DbString "Goodbye world!"
  , DbNumber 9002
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr mapToTime [] where
  mapToTime (DbDate t) ts = t : ts
  mapToTime _          ts = ts

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr mapToInteger [] where
  mapToInteger (DbNumber n) ns = n : ns
  mapToInteger _            ns = ns

filterDbString :: [DatabaseItem] -> [String]
filterDbString = foldr mapToString [] where
  mapToString (DbString s) ss = s : ss
  mapToString _            ss = ss

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = minimum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb dis = dbSum / amount
 where
  dbSum  = fromIntegral $ sumDb dis
  amount = fromIntegral $ length $ filterDbNumber dis

foldsMain :: IO ()
foldsMain = do
  putStrLn "Folds:"
  putStrLn $ "theDatabase: " ++ show theDatabase
  putStrLn $ "filterDbDate: " ++ show (filterDbDate theDatabase)
  putStrLn $ "filterDbInteger: " ++ show (filterDbNumber theDatabase)
  putStrLn $ "filterDbString: " ++ show (filterDbString theDatabase)
  putStrLn $ "mostRecent: " ++ show (mostRecent theDatabase)
  putStrLn $ "sumDb: " ++ show (sumDb theDatabase)
  putStrLn $ "avgDb: " ++ show (avgDb theDatabase)
  putStrLn "\n"
