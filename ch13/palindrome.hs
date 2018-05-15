import Control.Monad
import Data.Char
import System.Exit

palindrome :: IO ()
palindrome = forever $ do
  input <- getLine
  case (checkPalindrome input) of
    True -> putStrLn "It's a palindrome!"
    False ->
      exitSuccess

checkPalindrome :: String -> Bool
checkPalindrome "" = False
checkPalindrome input =
  lowerInput == reverse lowerInput
  where lowerInput = map toLower input
