import Data.Monoid
--sum' :: (Foldable t, Num a) => t a -> a
--sum' = foldr (+) 0

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' e f = getAny $ foldMap (\x -> Any $ e == x) f

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr maybeMin Nothing
  where maybeMin x Nothing = Just x
        maybeMin x (Just y) = Just (min x y)

null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True

length' :: (Foldable t) => t a -> Int
length' = foldr (\_ x -> x + 1) 0

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (\f list -> f : list) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\x y -> mappend (f x) y) mempty



-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

-- 1 --
data Constant a b = Constant b

instance Foldable (Constant a) where
  foldr _ x _ = x

-- 2 --
data Two a b = Two a b

instance Foldable (Two a) where
  foldr f x (Two a b) = f b x

-- 3 --
data Three a b c =
  Three a b c

instance Foldable (Three a b) where
  foldr f x (Three a b c) = f c x

-- 4 --
data Three' a b =
  Three' a b b

instance Foldable (Three' a) where
  foldr f x (Three' a b c) = f b (f c x)

-- 5 --
data Four' a b =
  Four' a b b b

instance Foldable (Four' a) where
  foldr f x (Four' a b c d) = f b $ f c $ f d x

-- Thinking cap --
filterF :: ( Applicative f , Foldable t
           , Monoid (f a))
  => (a -> Bool) -> t a -> f a
filterF f foldable = foldr (\e b -> if f e then (pure e) else mempty <> b) mempty foldable
