import Data.Char
import Data.List

data DaPhone = DaPhone [DaNumber]
data DaNumber = DaNumber Digit String

convo :: [String]
convo =
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever tasted alcohol",
   "Lol ya",
   "Wow ur cool haha. Ur turn",
   "Ok. Do u think I am pretty Lol",
   "Lol ya",
   "Just making sure rofl ur turn"]

phoneNumbers :: [DaNumber]
phoneNumbers = [DaNumber '0' " 0",
                DaNumber '1' "1",
                DaNumber '2' "abc2",
                DaNumber '3' "def3",
                DaNumber '4' "ghi4",
                DaNumber '5' "jkl5",
                DaNumber '6' "mno6",
                DaNumber '7' "pqrs7",
                DaNumber '8' "tuv8",
                DaNumber '9' "wxyz9",
                DaNumber '*' "^*",
                DaNumber '#' ".,"]

daPhone = DaPhone phoneNumbers

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone []) _ = []
reverseTaps (DaPhone ((DaNumber digit chars):nums)) char
  | ' ' == char = [('0', 1)]
  | ord 'z' < ord char = []
  | ord 'a' <= ord char = case (elemIndex char chars) of
      Just index -> [(digit, index + 1)]
      Nothing -> reverseTaps (DaPhone nums) char
  | ord 'Z' < ord char = []
  | ord 'a' <= ord char
    || ord 'A' <= ord char
    || (ord '0' <= ord char && ord char <= ord '9')
    || elem char "^*.,"
  = case (elemIndex char chars) of
      Just index -> [('*', 1), (digit, index + 1)]
      Nothing -> reverseTaps (DaPhone nums) char

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone string
  = concat $ map (\char -> reverseTaps phone char) string

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps tuples = foldr (\(_, presses) sum -> presses + sum) 0 tuples

mostPopularLetter :: String -> Char
mostPopularLetter input = fst (mostCommon (sort input) 0 (head input))
  where
    
mostCommon :: String -> Int -> Char -> (Char, Int)
mostCommon "" currMax currChar = (currChar, currMax)
mostCommon list prevMax prevChar
  | prevMax < currMax = mostCommon newList currMax currChar
  | otherwise = mostCommon newList prevMax prevChar
  where
    currChar = head list
    newList = dropWhile (\char -> char == currChar) (sort list)
    currMax = (length list) - (length newList)
        
coolestLetter :: [String] -> Char
coolestLetter input = fst (foldr (\string tuple -> (maxTuple (mostCommon string 0 'a') tuple)) ('a', 0) input)
  where
    maxTuple (charA, valA) (charB, valB)
      | valA < valB = (charB, valB)
      | otherwise = (charA, valA)

-- coolestWord :: [String] -> Char
-- coolestWord input =
