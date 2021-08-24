module Modules.Char where

import           Data.Char
import           Data.Function
import           Data.List

sentence :: String
sentence = "hey guys it's me"

words' :: [String]
words' = filter (not . all isSpace) $ groupBy ((==) `on` isSpace) sentence

ord' :: [Int]
ord' = map ord "heyhooray"

-- easier cipher example
encode :: Int -> String -> String
encode shift msg = map (chr . (+ shift) . ord) msg
  -- let ords    = map ord msg
  --     shifted = map (+ shift) ords
  -- in  map chr shifted

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg
-- decode shift msg = map (chr . subtract shift . ord) msg
  -- let ords    = map ord msg
  --     shifted = map (subtract shift) ords
  -- in  map chr shifted
