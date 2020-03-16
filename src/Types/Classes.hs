module Types.Classes where

class (Num a) => ExampleClass a where
  lessByTwo :: a -> a

newtype ExampleData a = ExampleData a deriving (Show)

instance Num a => Num (ExampleData a) where
  (+) (ExampleData x) (ExampleData x') = ExampleData $ x + x'
  (*) (ExampleData x) (ExampleData x') = ExampleData $ x * x'
  negate (ExampleData x) = ExampleData $ negate x
  abs (ExampleData x) = ExampleData $ abs x
  signum (ExampleData x) = ExampleData $ signum x
  fromInteger x = ExampleData $ fromInteger x

instance Num a => ExampleClass (ExampleData a) where
  lessByTwo (ExampleData x) = ExampleData $ x - 2

instance ExampleClass Int where
  lessByTwo x = x - 2

instance ExampleClass Integer where
  lessByTwo x = x - 2

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show)

instance TooMany Goats where
  tooMany (Goats g) = tooMany g

newtype StrInt = StrInt (Int, String)

newtype IntInt = IntInt (Int, Int)

newtype NumInt a = NumInt (a, a)

instance TooMany StrInt where
  tooMany (StrInt (i, _)) = tooMany i

instance TooMany IntInt where
  tooMany (IntInt (i, i')) = tooMany $ i + i'

instance (Num a, TooMany a) => TooMany (NumInt a) where
  tooMany (NumInt (i, i')) = tooMany $ i + i'
