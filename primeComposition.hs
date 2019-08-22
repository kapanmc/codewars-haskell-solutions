import Data.List

-- primes = map head $ iterate (\(x:xs) -> [y | y<-xs, y `mod` x /= 0 ]) [2..]


prime_factors' :: Integer -> [Integer]
prime_factors' 1 = []
prime_factors' n
  | factors == []  = [n]
  | otherwise = factors ++ prime_factors' (n `div` (head factors))
  where factors = take 1 $ filter ((==0) . mod n) [2 .. round $ sqrt $ fromIntegral n]


--   map (\x -> "(" ++ show $ head x ++ if length x > 1 then "**" ++ show $ length x  else ")") grp

primeFormat :: [Integer] -> [Char]
primeFormat n
    | length n == 1 = "(" ++ show (head n) ++ ")"
    | otherwise = "(" ++ show (head n) ++ "**" ++ show (length n) ++ ")"


prime_factors :: Integer -> String
prime_factors n = concat $ map primeFormat grouped
    where grouped = group $ prime_factors' n


-- alternative solutions



-- import           Control.Arrow ((&&&))
-- import           Data.List     (group, (\\))

-- prime_factors :: Integer -> String
-- prime_factors = printFac . map (head &&& length) . group . factor

-- -- https://wiki.haskell.org/99_questions/Solutions/35
-- factor 1 = []
-- factor n = let divisors = dropWhile ((/= 0) . mod n) [2 .. ceiling $ sqrt $ fromIntegral n]
--            in let prime = if null divisors then n else head divisors
--               in (prime :) $ factor $ div n prime

-- printFac :: [(Integer, Int)] -> String
-- printFac = foldl f "" where
--   f str (base, power) = str ++ "(" ++ show base ++
--    (if power == 1 then "" else "**" ++ show power) ++ ")"
-- Best Practices5Clever0
-- 0ForkCompare with your solutionLink


-- module Codewars.Kata.PrFactors where
--     import Data.List
    
--     prFactors :: Integer -> [Integer]
--     prFactors nb = prFactors' nb 2
--       where
--         prFactors' nb fact
--           | fact * fact > nb    = [nb]
--           | nb `mod` fact == 0  = fact : prFactors' (nb `div` fact) fact
--           | otherwise           = prFactors' nb (fact + 1)
    
--     prime_factors_str :: Integer -> [String]     
--     prime_factors_str = map mkpair . group . prFactors
--         where mkpair xs | (length xs) > 1 = "(" ++ (show (head xs)) ++ "**" ++ (show (length xs)) ++ ")"
--                         | otherwise = "(" ++ show (head xs) ++ ")"
    
--     prime_factors :: Integer -> String  
--     prime_factors n = intercalate "" (prime_factors_str n)