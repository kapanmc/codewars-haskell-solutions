

-- fibonacciSeries :: Int -> Int
-- fibonacciSeries 0 = 0
-- fibonacciSeries 1 = 1
-- fibonacciSeries n = fibonacciSeries(n-1) + fibonacciSeries (n-2)



perimeter :: Integer -> Integer
perimeter 0 = 0
perimeter n = (*4) $ sum $ take (fromIntegral n +1) fibs


fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)