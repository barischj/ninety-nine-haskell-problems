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

-- | Question 16.
dropEvery :: [a] -> Int -> [a]
dropEvery xs n =
  map snd $ filter ((/= n) . fst) (zip (cycle [1..n]) xs)

-- | Question 17.
split :: [a] -> Int -> ([a], [a])
split []     _ = ([],   [])
split xs     0 = ([],   xs)
split (x:xs) n = (x:x1, x2)
  where (x1, x2) = split xs (n - 1)

-- Question 18.
slice :: Int -> Int -> [a] -> [a]
slice start stop = take (stop - start + 1) . drop (start - 1)

-- Question 19.
rotate :: Int -> [a] -> [a]
rotate n xs
  | n > 0     = drop n xs ++ take n xs
  | otherwise = drop (length xs + n) xs ++ take (length xs + n) xs

-- Lovely alternative.
rotate' :: Int -> [a] -> [a]
rotate' n xs = take (length xs) $ drop (length xs + n) $ cycle xs

-- Question 20.
removeAt :: [a] -> Int -> [a]
removeAt xs n = start ++ drop 1 end
  where (start, end) = splitAt n xs
