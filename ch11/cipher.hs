import Data.Char
import Data.List

vigenere :: String -> String -> String
vigenere text secret = intercalate " " (vig (words text) (cycle secret))
  where
    vig [] _ = []
    vig (word:wordList) cycledSecret
      = [cipher word (take (length word) cycledSecret)] ++ (vig wordList (drop (length word) cycledSecret))
    cipher word secret
      = zipWith (\char offset -> shift (+) char offset) word secret
-- 
-- unvigenere :: String -> String -> String
-- unvigenere secret = zipWith (shift (-)) (cycle secret) . concat . words
-- 
shift :: (Int -> Int -> Int) -> Char -> Char -> Char
shift op ch offset = numToChar $ (charToNum . toUpper $ ch) `op` (charToNum . toUpper $ offset)
  where
    charToNum ch = ord ch - ord 'A'
    numToChar n = chr $ (n `mod` 26) + ord 'A'

