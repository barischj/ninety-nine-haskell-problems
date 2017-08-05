module Questions_1_10 where

import           Safe (headMay)

-- | Question 1.
myLast :: Foldable f => f a -> Maybe a
myLast = foldl (\_ x -> Just x) Nothing

-- | Question 2.
myButLast :: Foldable f => f a -> Maybe a 
myButLast = fst . foldl (\(x, y) z -> (y, Just z)) (Nothing, Nothing)

-- | Question 3.
elementAt :: [a] -> Int -> Maybe a
elementAt xs n
  | n < 1     = Nothing
  | otherwise = headMay $ drop (n - 1) xs

-- | Question 4.
myLength :: Num b => Foldable f => f a -> b
myLength = foldl (\l _ -> l + 1) 0

-- | Question 5.
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- | Question 6.
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- | Question 7.

data Tree a = Leaf a | Node [Tree a]

flatten :: Tree a -> [a]
flatten (Leaf x)  = [x]
flatten (Node xs) = concatMap flatten xs

concatMap_ :: (a -> [b]) -> [a] -> [b]
concatMap_ f = concat . map f

concat_ :: [[a]] -> [a]
concat_ []     = []
concat_ (x:xs) = x ++ concat_ xs

compress :: Eq a => [a] -> [a]
compress xs = compress' xs Nothing

compress' :: Eq a => [a] -> Maybe a -> [a]
compress' [] _ = []
compress' (x:xs) Nothing = x : compress' xs (Just x)
compress' (x:xs) (Just c)
  | x == c    =     compress' xs (Just c)
  | otherwise = x : compress' xs (Just x)

-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- b = (Maybe a, a)
-- compress'' = foldr cons
--   where cons x Nothing  = [x]
--         cons x (Just c)
--           | x == c =


