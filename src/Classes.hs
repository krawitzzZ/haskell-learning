module Classes where

class (Num a) => ExampleClass a where
  lessByTwo :: a -> a

-- newtype ExampleData a = ExampleData a deriving (Show)
data ExampleData a = ExampleData a deriving (Show)

instance Num a => Num (ExampleData a) where
  (+) (ExampleData x) (ExampleData x') = ExampleData $ x + x
  (*) (ExampleData x) (ExampleData x') = ExampleData $ x * x
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
