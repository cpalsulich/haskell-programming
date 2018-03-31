data Nat =
  Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nat) = 1 + natToInteger nat

integerToNat :: Integer -> Maybe Nat
integerToNat int
  | int < 0 = Nothing
  | otherwise = Just (integerToNat' int)
  where
    integerToNat' 0 = Zero
    integerToNat' val = Succ (integerToNat' (val - 1))

