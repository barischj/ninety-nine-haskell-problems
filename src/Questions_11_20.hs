module Questions_11_20 where

import qualified Questions_1_10 as Q_1_10

-- | Question 11.
data Encoded a = Multiple Int a | Single a

encodeModified :: Eq a => [a] -> [Encoded a]
encodeModified = map f . Q_1_10.encode
  where f (n, x)
          | n == 1    = Single x
          | otherwise = Multiple n x

-- | Question 12.
decodeModified :: [Encoded a] -> [a]
decodeModified = concatMap f
  where f (Single     x) = [x]
        f (Multiple n x) = replicate n x

-- | Question 13.
encodeDirect :: Eq a => [a] -> [Encoded a]
encodeDirect [] = []
encodeDirect (e:xs) = currE:encodeDirect notEs
  where (es, notEs) = span (== e) xs
        currE = case length (e:es) of
          1 -> Single     e
          l -> Multiple l e

-- | Question 14.
dupli :: [a] -> [a]
dupli (x:xs) = x:x:dupli xs

-- | Question 15.
repli :: [a] -> Int -> [a]
repli xs n = foldl (\acc e -> acc ++ repliX e n) [] xs
  where repliX _ 0 = []
        repliX x n = x:repliX x (n - 1)
