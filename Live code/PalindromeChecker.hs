module PalindromeChecker where

checkPalindrome :: Eq a => [a] -> [Char]
checkPalindrome s
  | s == reverse s = "palindrome!"
  | otherwise      = "not a palindrome..." 

main :: IO ()
main = do
  input <- getContents
  putStr $ unlines 
         $ map checkPalindrome 
         $ lines input

-- or: main = interact (map checkPalindrome)