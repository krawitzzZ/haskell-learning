module Monads.Transformers where

import           Control.Monad.Error
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Map                      as Map
import           Data.Maybe


-- variable names
type Name = String

-- expressions
data Exp = Lit Integer | Var Name | Plus Exp Exp | Abs Name Exp | App Exp Exp deriving (Show)

-- values
data Value = IntVal Integer | FunVal Env Name Exp deriving (Show)

-- mapping from names to values
type Env = Map.Map Name Value

eval0 :: Env -> Exp -> Value
eval0 _env (Lit i) = IntVal i
eval0 env  (Var n) = fromJust (Map.lookup n env)
eval0 env (Plus e1 e2) =
  let IntVal i1 = eval0 env e1
      IntVal i2 = eval0 env e2
  in  IntVal (i1 + i2)
eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2) =
  let val1 = eval0 env e1
      val2 = eval0 env e2
  in  case val1 of
        FunVal env0 n body -> eval0 (Map.insert n val2 env0) body
