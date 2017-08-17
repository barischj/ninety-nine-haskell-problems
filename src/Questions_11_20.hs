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
  where f (Single    x) = [x]
        f (Multiple n x) = replicate n x
