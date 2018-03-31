import Data.Either

lefts' :: [Either a b] -> [a]
lefts' list = foldr getLefts [] list
  where
    getLefts :: (Either a b) -> [a] -> [a]
    getLefts (Right _) list = list
    getLefts (Left a) list = a : list

rights' :: [Either a b] -> [b]
rights' list = foldr getRights [] list
  where
    getRights :: (Either a b) -> [b] -> [b]
    getRights (Left _) list = list
    getRights (Right b) list = b : list

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' list = foldr split ([], []) list
  where
    split :: (Either a b) -> ([a], [b]) -> ([a], [b])
    split (Left a) (lefts, rights) = (a : lefts, rights)
    split (Right b) (lefts, rights) = (lefts, b : rights)

eitherMaybe' :: (b -> c)
             -> Either a b
             -> Maybe c
eitherMaybe' f (Left _) = Nothing
eitherMaybe' f (Right b) = Just (f b)

either' :: (a -> c)
        -> (b -> c)
        -> Either a b
        -> c
either' aToC _ (Left a) = aToC a
either' _ bToC (Right b) = bToC b

eitherMaybe'' :: (b -> c)
              -> Either a b
              -> Maybe c
eitherMaybe'' f either = either' (\_ -> Nothing) (\b -> Just (f b)) either
