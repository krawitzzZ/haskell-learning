module Monads.HelperFuncs where

import Control.Arrow
import Control.Monad
import Data.Char (toUpper)

fmapped :: Maybe Integer
fmapped = fmap (* 3) (Just 8)

-- liftM is the same as fmap, but just for Monads
liftedM :: Maybe Integer
liftedM = liftM (* 3) (Just 8)
