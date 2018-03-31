module Arith4 where

-- id :: a -> a
-- id x = x

roundTrip :: (Show a, Read a) => a -> a
roundTrip = read . show
-- roundTrip a = read (show a)

roundTrip2 :: (Show a, Read b) => a -> b
roundTrip2 = read . show

main = do
  print (roundTrip 4)
  print ((roundTrip2 4) :: Int)
  print (id 4)
