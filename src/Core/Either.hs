module Core.Either where

import           Core.Basics                    ( Name
                                                , Age
                                                , Person(..)
                                                )

data PersonInvalid = EmptyName | LowAge deriving (Show, Eq, Ord)

type ValidatePerson a = Either [PersonInvalid] a

checkAge :: Age -> Either [PersonInvalid] Age
checkAge age = case age >= 0 of
  True  -> Right age
  False -> Left [LowAge]

checkName :: Name -> Either [PersonInvalid] Name
checkName ""   = Left [EmptyName]
checkName name = Right name

mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age = mkPerson' (checkName name) (checkAge age)
 where
  mkPerson'
    :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
  mkPerson' (Right nameOk ) (Right ageOk ) = Right $ Person nameOk ageOk
  mkPerson' (Left  badName) (Left  badAge) = Left $ badName ++ badAge
  mkPerson' (Left  badName) _              = Left badName
  mkPerson' _               (Left badAge)  = Left badAge
