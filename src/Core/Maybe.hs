module Core.Maybe where

import           Core.Basics                    ( Name
                                                , Age
                                                , Person(..)
                                                )

mkPerson :: Name -> Age -> Maybe Person
mkPerson name age | name /= "" && age >= 0 = Just $ Person name age
                  | otherwise              = Nothing
