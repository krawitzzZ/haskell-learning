module Core.Maybe where

import qualified Data.Maybe                    as Mb

import           Core.Basics                    ( Name
                                                , Age
                                                , Person(..)
                                                )

mkPerson :: Name -> Age -> Maybe Person
mkPerson name age | name /= "" && age >= 0 = Just $ Person name age
                  | otherwise              = Nothing

notThe :: String -> Maybe String
notThe the = if the == "the" then Nothing else Just the

-- >>> replaceThe "the cow loves us"
-- "a cow loves us"
replaceThe :: String -> String
replaceThe str = concat $ intercalate' " " $ map replaceThe' (splitWords str)
 where
  replaceThe' w = Mb.fromMaybe "a" (notThe w)
  splitWords []           = []
  splitWords (' ' : str') = splitWords str'
  splitWords str' =
    takeWhile (/= ' ') str' : splitWords (dropWhile (/= ' ') str')
  intercalate' _   []       = []
  intercalate' _   [w     ] = [w]
  intercalate' sep (w : ws) = [w, sep] ++ intercalate' sep ws
