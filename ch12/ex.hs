import Data.List
import Data.Char

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe val = Just val

replaceThe :: String -> String
replaceThe input = intercalate " " $ map (\word -> if word == "the" then "a" else word) (words input)

replaceThe' :: String -> String
replaceThe' input = rep (words input)
  where
    rep [] = ""
    rep (word:words) = concat [convWord . notThe $ word, " ", rep words]
    convWord Nothing = "a"
    convWord (Just val) = val

vowels = "aeiou"

startsWithVowel (x:xs) = elem (toLower x) vowels
      
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel input = getCount (words input) False 0
  where
    getCount [] _ count = count
    getCount ("the":words) _ count = getCount words True count
    getCount (word:words) prevThe count
      | prevThe == True && startsWithVowel word = getCount words False (count + 1)
      | otherwise = getCount words False count

countVowels :: String -> Integer
countVowels "" = 0
countVowels (x:xs)
  | elem x vowels = 1 + countVowels xs
  | otherwise = countVowels xs

newtype Word' =
  Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord input = if (fst vowelConsonants) > (snd vowelConsonants) then Nothing else Just (Word' input)
  where
    vowelConsonants = getCounts input 0 0
    getCounts :: String -> Int -> Int -> (Int, Int)
    getCounts "" vows cons = (vows, cons)
    getCounts (x:xs) vows cons =
      if (elem x vowels)
      then getCounts xs (vows + 1) cons
      else getCounts xs vows (cons + 1)
