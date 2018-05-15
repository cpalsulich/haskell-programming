type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
  NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =
    Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
    Left $ PersonInvalidUnknown $
    "Name was: " ++ show name ++
    " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  name <- readString "What is your name?"
  age <- readInt "What is your age?"
  case (mkPerson name age) of
    Right (Person n a) ->
      putStr ("Yay! Successfully got a person: " ++ name ++ " " ++ (show age))
    Left NameEmpty ->
      putStr "Name not provided"
    Left AgeTooLow ->
      putStr "Age too low"
    Left (PersonInvalidUnknown msg) -> 
      putStr ("Unknown error: " ++ msg)

readString :: String -> IO String
readString msg = do
  putStr msg
  input <- getLine
  return (input)
    

readInt :: String -> IO Integer
readInt msg = do
  putStr msg
  input <- getLine
  return (read input)
    
