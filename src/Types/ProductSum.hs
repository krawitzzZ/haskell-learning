module Types.ProductSum where

data OperatingSystem =
  Linux
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgrammingLanguage =
  Haskell
  | JavaScript
  | Go
  deriving (Eq, Show)

data Programmer = Programmer { os :: OperatingSystem , lang :: ProgrammingLanguage } deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [Linux, Mac, Windows]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, JavaScript, Go]

allProgrammers :: [Programmer]
allProgrammers =
  [ Programmer { os = system, lang = language }
  | system   <- allOperatingSystems
  , language <- allLanguages
  ]
