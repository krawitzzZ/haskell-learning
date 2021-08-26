module IOO.Palindrome.Palindrome where


main :: IO ()
main = do
  print "Let's start"
  interact palindrome

palindrome :: String -> String
palindrome = unlines . map toMaybePalindrome . lines
 where
  toMaybePalindrome s = if s == reverse s
    then s ++ " is a palindrome"
    else s ++ " is not a palindrome"
