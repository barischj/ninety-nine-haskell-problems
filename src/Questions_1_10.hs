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
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x)  = [x]
flatten (List ls) = concatMap flatten ls

-- | Question 8.
compress :: Eq a => [a] -> [a]
compress []     = []
compress (x:xs) = x : compress (dropWhile (== x) xs)

-- | Question 9.
pack :: Eq a => [a] -> [[a]]
pack []     = []
pack (e:xs) = (e : es) : pack notEs
  where (es, notEs) = span (== e) xs

-- Question 10.
encode :: Eq a => [a] -> [(Int, a)]
encode = map f . pack
  where f x = (length x, head x)
