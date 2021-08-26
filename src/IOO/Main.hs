module IOO.Main where

import           Data.Char
import           Control.Monad                  ( when
                                                , forever
                                                , forM
                                                )

capsName :: IO ()
capsName = do
  putStrLn "what is your first name?"
  name <- getLine
  putStrLn "what is your last name?"
  lastName <- getLine
  let bigFirstName = map toUpper name
      bigLastName  = map toUpper lastName
  putStrLn $ "HEY! " ++ bigFirstName ++ " " ++ bigLastName ++ "\n"

infiniteReverse :: IO ()
infiniteReverse = do
  line <- getLine
  if null line
    then return ()
    else do
      putStrLn $ reverseWords line ++ "\n"
      infiniteReverse
  where reverseWords = unwords . map reverse . words

readChar :: IO ()
readChar = do
  c <- getChar
  -- if c /= ' '
  --   then do
  --     putChar c
  --     readChar
  --   else return ()
  when (c /= ' ') $ do
    putChar c
    putStrLn "\n"
    readChar


-- sequence executes a list of IO actions as one IO action
-- mapM_ then executes a `a -> IO` function on every element in result
-- mapM does not discard the result of map and return the resulting list
doSequence :: IO ()
doSequence = do
  -- result <- sequence [return "And this is what we've got", getLine, getLine]
  -- mapM_ print result
  -- _ <- sequence $ map print ["hey", "there", "how", "are", "you", "?"] -- or like this
  -- return ()
  mapM_ print ["hey", "there", "how", "are", "you", "?"] -- or even like this

foreva :: IO b
foreva = forever $ do
  putStrLn "What do you want to say?"
  line <- getLine
  putStrLn $ map toUpper line


-- forM is like mapM, but arguments are flipped
forThing :: IO ()
forThing = do
  result <- forM
    ["1", "2", "3"]
    (\x -> do
      putStrLn $ "what shall we do as point " ++ x ++ " today?"
      -- also possible to just end the line with `getLine` which will just return whatever input was there
      todo <- getLine
      return (x, todo)
    )
  putStrLn "and here goes your todo list"
  mapM_ (\(num, todo) -> print $ num ++ ": " ++ todo) result
