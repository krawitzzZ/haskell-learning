module Core.Maybe where

import qualified Data.Maybe                    as Mb

import           Core.Basics                    ( Name
                                                , Age
                                                , Person(..)
                                                )

mkPerson :: Name -> Age -> Maybe Person
mkPerson name age | name /= "" && age >= 0 = Just $ Person name age
                  | otherwise              = Nothing

splitWords :: String -> [String]
splitWords []           = []
splitWords (' ' : str') = splitWords str'
splitWords str' =
  takeWhile (/= ' ') str' : splitWords (dropWhile (/= ' ') str')

isVowel :: Char -> Bool
isVowel c = c `elem` ['a', 'e', 'i', 'o', 'u', 'y']

-- isThe :: String -> Bool
-- isThe the = the == "the"

notThe :: String -> Maybe String
notThe the = if the == "the" then Nothing else Just the

-- >>> replaceThe "the cow loves us"
-- "a cow loves us"
replaceThe :: String -> String
replaceThe str = concat $ intercalate' " " $ map replaceThe' (splitWords str)
 where
  replaceThe' w = Mb.fromMaybe "a" (notThe w)
  intercalate' _   []       = []
  intercalate' _   [w     ] = [w]
  intercalate' sep (w : ws) = [w, sep] ++ intercalate' sep ws

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel str = countTheBeforeVowel' $ splitWords str
 where
  countTheBeforeVowel' []  = 0
  countTheBeforeVowel' [_] = 0
  countTheBeforeVowel' (w : w' : ws) =
    let count = if isThe && isVowel (head w') then 1 else 0
        isThe = case notThe w of
          Nothing -> True
          _       -> False
        rest = countTheBeforeVowel' (w' : ws)
    in  count + rest

countVowels :: String -> Integer
countVowels str = fromIntegral $ length $ filter isVowel str
