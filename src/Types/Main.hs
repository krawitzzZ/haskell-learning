module Types.Main where
  -- ( Shape(Rectangle, Circle)
  -- , Point(..)
  -- , surface
  -- , nudge
  -- , baseCircle
  -- , baseRect
  -- , person
  -- )

import qualified Data.Map                      as Map


-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show, Eq)
data Point = Point Float Float deriving (Show, Eq)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show, Eq)


surface :: Shape -> Float
surface (Circle    _             r            ) = pi * r ^ (2 :: Integer)
surface (Rectangle (Point x1 y1) (Point x2 y2)) = x * y
 where
  x = abs (x2 - x1)
  y = abs (y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b =
  Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect x y = Rectangle (Point 0 0) (Point x y)


-- record syntax

data Person = Person {
  firstname::String,
  lastName :: String,
  age:: Int,
  height :: Int,
  phoneNumber:: String,
  favFlavor :: String
} deriving (Eq, Show, Read, Ord)

person :: Person
person = Person { firstname   = "John"
                , lastName    = "Doe"
                , age         = 30
                , height      = 180
                , phoneNumber = "123456789"
                , favFlavor   = "vanilla"
                }

data Vector a = Vector a a a deriving (Eq, Show)

vplus :: Num t => Vector t -> Vector t -> Vector t
(Vector x y z) `vplus` (Vector a b c) = Vector (x + a) (y + b) (z + c)

vmult :: Num t => Vector t -> Vector t -> Vector t
(Vector x y z) `vmult` (Vector a b c) = Vector (x * a) (y * b) (z * c)

vscalarmult :: Num t => Vector t -> Vector t -> t
(Vector x y z) `vscalarmult` (Vector a b c) = x * a + y * b + z * c


data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Show, Read, Ord, Enum, Bounded)


-- type synonyms

type PhoneBook = [(String, String)]

phoneBook :: PhoneBook
phoneBook =
  [ ("betty"  , "555-2938")
  , ("bonnie" , "452-2928")
  , ("patsy"  , "493-2928")
  , ("lucille", "205-2928")
  , ("wendy"  , "939-8282")
  , ("penny"  , "853-2492")
  ]

-- type IntMap v = Map.Map Int v
type IntMap = Map.Map Int -- IntMap is a type constructor function that will return a concrete type

intMap :: IntMap String
intMap = Map.singleton 1 "something"

-- with Either Left is used for errors and Right for successful result
someEitherThing :: Int -> Either String String
someEitherThing x = if x > 0 then Right "that works" else Left "not this time"

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup k lMap = case Map.lookup k lMap of
  Just (Free , code) -> Right code
  Just (Taken, _   ) -> Left "sorry dude, this one is taken"
  Nothing            -> Left "are you kidding? there is no such locker!"

lockerMap :: LockerMap
lockerMap =
  Map.fromList [(1, (Free, "123")), (2, (Taken, "234")), (3, (Free, "567"))]


-- recursive types

-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty      .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)


data Sample = Sample { a :: Int, b :: Int, c :: (Int, Int) } deriving (Eq, Show)

sample :: Sample
sample = Sample 1 2 (1, 2) -- Sample { a = 1, b = 2, c = (1, 2) }

data Sample' a b  = Sample' { b' :: b, a' :: a } deriving (Eq, Show)

sample' :: Sample' Int String
-- here string goes first, because in record syntax b' goes first (which is type String in `Sample' Int String`)
sample' = Sample' "String" 2  -- Sample' { a' = 2, b' = "String" }
