module Types.Class where

import           Data.Char                      ( ord )
import           Types.BinaryTree               ( Tree(..) )


data TrafficLight = Red | Yellow | Green

-- define a custom typeclass
class Stringer a where
  toString :: a -> String
  -- it is also possible to have default implementation
  toString _ = "Of course I can get you some string, but you'd better implement this class yourself"

-- define an instance of Eq typeclass for TrafficLight data type
instance Stringer TrafficLight where
  toString Red    = "It's Red! STOP!"
  toString Yellow = "It's Yellow! Just wait a little bit more"
  toString Green  = "It's Green! Go! Go! Go!"

-- or instance that uses default implementation from typeclass definition
-- instance Stringer TrafficLight

-- define an instance of Eq typeclass for TrafficLight data type
instance Eq TrafficLight where
  Red    == Red    = True
  Yellow == Yellow = True
  Green  == Green  = True
  _      == _      = False

instance Show TrafficLight where
  show Red    = "Red light"
  show Yellow = "Yellow light"
  show Green  = "Green light"


-- example of inclusion of typeclasses
class Foo a where
    foo :: a -> Int

class Foo a => Bar a where
    bar :: a -> [Int]
    bar x = [foo x]

instance Foo Char where
  foo = ord

-- instance with default 'bar' implementation
instance Bar Char


-- example for types with type variables
data Probably a = Nope | Exactly a

instance Eq a => Eq (Probably a) where
  (Exactly x) == (Exactly y) = x == y
  Nope        == Nope        = True
  _           == _           = False


-- functor instance
instance Functor Probably where
  fmap _ Nope        = Nope
  fmap f (Exactly a) = Exactly (f a)

-- Functor instance for Tree. and yes, linter complains for a reason
instance Functor Tree where
  fmap _ Leaf                = Leaf
  fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

-- kind of Either data type
data OneOrAnother a b = One a | Another b deriving (Show, Eq, Ord)

instance Functor (OneOrAnother a) where
  fmap _ (One     x) = One x
  fmap f (Another x) = Another (f x)


-- some more stuff about types and kinds
class Tofu t where
  tofu :: j a -> t a j

newtype Frank a b = Frank { frankField :: b a } deriving (Show, Eq)

instance Tofu Frank where
  tofu f = Frank f

data Barry a b c = Barry { yabba:: a b, dabba :: c } deriving (Show, Eq)

barry :: Barry ((->) a) (Maybe a) String
barry = Barry { yabba = Just, dabba = "dabba" }

-- since we partially applied Barry, `a` :: (* -> *) and `b` :: (*) will stay the same and we only need to change `c` (which is dabba here)
instance Functor (Barry a b) where
  fmap f Barry { yabba = x, dabba = y } = Barry { yabba = x, dabba = f y }

data Dudello a b c = Dudello { kindOfType :: a b, justField :: c } deriving (Show, Eq)

dudello :: Dudello Maybe Char Integer
dudello = Dudello { justField = 1 :: Integer, kindOfType = Just 'a' }
