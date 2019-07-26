import Data.List

getUnique :: [Int] -> Int
getUnique n = head $ concat $ filter (\x -> length x == 1) (group $ sort n)

-- ALTERNATIVE Solutions

-- module CodeWars.UniqueNumber where

-- import Data.List (find)
-- import Data.Maybe (fromJust)

-- getUnique :: [Int] -> Int
-- getUnique xs@(x:y:z:_) = fromJust $ find (/= if x == y then x else z) xs 


-- module CodeWars.UniqueNumber where

-- getUnique :: [Int] -> Int
-- getUnique (x:xs)
--   | x `elem` xs = getUnique $ filter (/=x) xs
--   | otherwise = x


-- module CodeWars.UniqueNumber where

-- getUnique :: [Int] -> Int
-- getUnique r@(a:b:c:_) | a == b = head $ filter (a /=) r
--                       | a == c = b
--                       | True   = a
