module Problems.Rpn where


expression :: String
expression = "10 4 3 + 2 * -"

operators :: (Fractional a, Read a) => [(String, a -> a -> a)]
operators = [("-", subtract), ("+", (+)), ("*", (*)), ("/", (/))]

solveRpn :: (Fractional a, Read a) => String -> a
solveRpn = head . foldl goThroughStack [] . words

goThroughStack :: (Fractional a, Read a) => [a] -> String -> [a]
goThroughStack stack el = case lookup el operators of
  Nothing   -> read el : stack
  Just func -> func x y : updatedStack where (x : y : updatedStack) = stack

-- this one is from book
solveRPN :: String -> Float
solveRPN = head . foldl foldingFunction [] . words
 where
  foldingFunction (x : y : ys) "*"       = (x * y) : ys
  foldingFunction (x : y : ys) "+"       = (x + y) : ys
  foldingFunction (x : y : ys) "-"       = (y - x) : ys
  foldingFunction (x : y : ys) "/"       = (y / x) : ys
  foldingFunction (x : y : ys) "^"       = (y ** x) : ys
  foldingFunction (x     : xs) "ln"      = log x : xs
  foldingFunction xs           "sum"     = [sum xs]
  foldingFunction xs           numberStr = read numberStr : xs
