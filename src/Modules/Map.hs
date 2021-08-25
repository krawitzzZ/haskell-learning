module Modules.Map where

import qualified Data.Map                      as Map

phoneBook :: [(String, String)]
phoneBook =
  [ ("betty"  , "555-2938")
  , ("bonnie" , "452-2928")
  , ("patsy"  , "493-2928")
  , ("lucille", "205-2928")
  , ("wendy"  , "939-8282")
  , ("penny"  , "853-2492")
  ]

extraPhoneBook :: [(String, String)]
extraPhoneBook =
  [ ("betty"  , "555-2938")
  , ("betty"  , "342-2492")
  , ("bonnie" , "452-2928")
  , ("patsy"  , "493-2928")
  , ("patsy"  , "943-2929")
  , ("patsy"  , "827-9162")
  , ("lucille", "205-2928")
  , ("wendy"  , "939-8282")
  , ("penny"  , "853-2492")
  , ("penny"  , "555-2111")
  ]


phoneBookMap :: Map.Map String String
phoneBookMap = Map.fromList phoneBook

-- findKey :: Eq a => a -> [(a, c)] -> c
-- findKey key xs = snd . head . L.filter (\(k, _) -> k == key) $ xs
findKey :: Eq k => k -> [(k, v)] -> Maybe v
-- findKey _   []            = Nothing
-- findKey key ((k, v) : xs) = if k == key then Just v else findKey key xs
findKey key = foldr (\(k, v) acc -> if k == key then Just v else acc) Nothing

getNum :: String -> Maybe String
getNum name = findKey name phoneBook

-- last pair will be ignored because the key is duplicated
someMap :: Map.Map Integer Integer
someMap =
  Map.insert 1 1 . Map.insert 2 2 . Map.insert 3 3 . Map.insert 1 4 $ Map.empty

maybeMapValue :: Maybe Integer
maybeMapValue = Map.lookup 1 someMap

isMember :: Bool
isMember = Map.member 1 someMap

listFromMap :: [(Integer, Integer)]
listFromMap = Map.toList someMap

toMap :: Ord k => [(k, String)] -> Map.Map k String
toMap xs = Map.fromListWith (\v1 v2 -> v1 ++ ", " ++ v2) xs

toBetterMap :: Ord k => [(k, String)] -> Map.Map k [String]
toBetterMap xs =
  Map.fromListWith (\v1 v2 -> v1 ++ v2) $ map (\(k, v) -> (k, [v])) xs
