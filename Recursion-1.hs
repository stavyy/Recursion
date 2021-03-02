{- 
Stavros Avdella 
3939968 
Spring 2019
Programming Languages
-}

{- isPrime Function -}

isqrt :: Integral i => i -> i
isqrt = floor . sqrt . fromIntegral

isPrime n = ip n [2..(isqrt n)]
  where
  ip _ [] = True
  ip n (x:xs)
    | n `mod` x == 0 = False
    | otherwise = ip n xs
  
{- factorial Function -}

factorial 0 = 1
factorial n = (n * factorial( n - 1))


{- fibonacci Function -}

fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)


