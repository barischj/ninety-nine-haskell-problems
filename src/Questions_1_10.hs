module Questions_1_10 where

import Data.Foldable (foldl')

-- | Question 1.
myLast :: [a] -> Maybe a
myLast [x] = Just x
myLast (_:xs) = myLast xs
myLast [] = Nothing

-- | Question 2.
myButLast :: [a] -> Maybe a
myButLast [x, _] = Just x
myButLast (_:xs) = myButLast xs
myButLast [] = Nothing

-- | Question 3.
elementAt :: (Eq a, Num a) => a -> [a] -> Maybe a
elementAt n (x:xs)
  | n == 0    = Just x
  | otherwise = elementAt (n - 1) xs
elementAt _ [] = Nothing

-- | Nth last element of a list. 0th last being last.
nthLast :: (Eq a, Num a) => a -> [a] -> Maybe a
nthLast n = elementAt n . reverse

-- | Question 4.
myLength :: Num b => [a] -> b
myLength = foldl' (\l _ -> l + 1) 0

-- | Question 5.
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

foldl_ :: (a -> b -> a) -> a -> [b] -> a
foldl_ _ n [] = n
foldl_ f n (x:xs) = foldl_ f (f n x) xs

flip_ :: (a -> b -> c) -> (b -> a -> c)
flip_ f b a = f a b

-- | Question 6.
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- | Question 7.

data Tree a = Leaf a | Node [Tree a]

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node xs) = concatMap flatten xs

concatMap_ :: (a -> [b]) -> [a] -> [b]
concatMap_ f = concat . map f

concat_ :: [[a]] -> [a]
concat_ [] = []
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

