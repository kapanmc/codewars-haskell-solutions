
digs :: Integer -> [Integer]
digs 0 = []
digs x
  | x < 0 = digs $ negate x
  | x > 0 = digs (div x 10) ++ [mod x 10]


multiplier = [1, 10, 9, 12, 3, 4]

thirt :: Integer -> Integer
thirt n 
    | n < 100 = n 
    | otherwise = thirt $ sum $ zipWith (*) (cycle multiplier) (reverse $ digs n)



-- ALTERNATIVE SOLUTIONS

-- module Codewars.G964.Thirteen where

-- thirt :: Integer -> Integer
-- thirt n
--   | n < 100   = n
--   | otherwise = thirt $ sum $ zipWith (*) (reverse (digits n)) rems where
--                   digits = map (read . return) . show
--                   rems = cycle [1, 10, 9, 12, 3, 4]


-- module Codewars.G964.Thirteen where
-- import Data.Char
 
-- pattern = cycle [1, 10, 9, 12, 3, 4] :: [Int]

-- digits :: Integer -> [Int]
-- digits = reverse . map digitToInt . show -- Obs, rightmost digit first.

-- zum :: Integer -> Integer
-- zum n = toInteger . sum $ zipWith (*) (digits n) pattern

-- thirt :: Integer -> Integer
-- thirt n = if zum n == n then n else thirt (zum n)

{- Short form but less readable:
thirt n = if zum n == n then n else thirt (zum n)
  where 
    zum n = toInteger . sum $ zipWith (*) (reverse . map digitToInt . show n) (cycle [1, 10, 9, 12, 3, 4])
-}


-- module Codewars.G964.Thirteen where

-- thirt :: Integer -> Integer
-- thirt = until (< 100) step
--   where
--     step = sum . zipWith (*) (cycle [1, 10, 9, 12, 3, 4]) . digits
--     digits 0 = []
--     digits n = let (d, m) = n `divMod` 10 in m : digits d