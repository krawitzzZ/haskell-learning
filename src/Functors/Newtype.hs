module Functors.Newtype where


-- data ZipList' a = ZipList' [a]
-- data ZipList' a = ZipList' { getZipList :: [a] }
newtype ZipList' a = ZipList' { getZipList :: [a] } deriving (Eq, Ord, Show)
-- newtype is used to just wrap some existing type in custom type
-- in order to make it an instance of some typeclass in a different way, for example

-- make a newtype out of tuple, so we can make a functor out of it
newtype Pair a b = Pair { getPair :: (a, b) } deriving (Eq, Ord, Show)

instance Functor (Pair a) where
  fmap f (Pair (a, b)) = Pair (a, f b)

-- switch parameters around so we can make functor out of it and change b, not a
newtype Pair' b a = Pair' { getPair' :: (a, b) } deriving (Eq, Ord, Show)

instance Functor (Pair' b) where
  fmap f (Pair' (a, b)) = Pair' (f a, b)

pair :: Pair Integer Integer
pair = fmap (+ 1) (Pair (1, 1)) -- Pair {getPair = (1,2)}

pair' :: Pair' Integer Integer
pair' = fmap (+ 1) (Pair' (1, 1)) -- Pair' {getPair' = (2,1)}

data CoolBool = CoolBool { getCoolBool :: Bool } deriving (Eq, Ord, Show)

-- this will throw an exception cuz haskell has to evaluate what's the type inside (because we used data keyword)
helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

newtype CoolBool' = CoolBool' { getCoolBool' :: Bool } deriving (Eq, Ord, Show)

-- this will work just fine, cuz haskell knows that we use Bool inside (because of newtype keyword)
helloMe' :: CoolBool' -> String
helloMe' (CoolBool' _) = "hello"
