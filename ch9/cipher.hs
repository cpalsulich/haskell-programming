module Cipher where

import Data.Char

caesar :: String -> Int -> String
caesar "" _ = ""
caesar input shift = map (\x -> chr ((mod ((ord (toLower x)) - (ord 'a') + shift) 26) + (ord 'a'))) input

unCaesar input shift = caesar input (-shift)
