module Arith3Broken where

main :: IO ()
main = do
  print "3"
  putStrLn "10"
  print (show (1))
  print (show ((+) 0 blah))
    where blah = negate 1
