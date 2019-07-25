module SquareDigit where

import Data.Char

-- digs :: Integer -> [Integer]
-- digs n = [toInteger (digitToInt x) | x <- show n]

squareDigit :: Integer -> Integer
squareDigit 0 = 0
squareDigit n --read $ concat $ map show $ map square $ digs n 
    | n < 0 = negate $ squareDigit $ negate n
    | n > 0 = read $ concat $ map show $ map square $ digs n 

square :: Integral a => a -> a
square a = a*a

--https://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell
-- digs :: Integral x => x -> [x]
-- digs 0 = []
-- digs x = digs (x `div` 10) ++ [x `mod` 10]



-- digs :: Integer -> [Integer]
-- digs = map (read . (:[])) . show

digs :: Integer -> [Integer]
digs 0 = []
digs x
  | x < 0 = digs $ negate x
  | x > 0 = digs (div x 10) ++ [mod x 10]




-- --ALTERNATIVE SOLUTIONS
-- module SquareDigit where
-- import Data.Char

-- squareDigit :: Int -> Int
-- squareDigit n
--     | n < 0 = negate (squareDigit (negate n))
--     | otherwise = read (show n >>= (show . (^2) . digitToInt))



-- module SquareDigit where
-- import Data.Char

-- squareDigit :: Int -> Int
-- squareDigit n = neg . read . concatMap ( show . (^2) . digitToInt ) . show . abs $ n
--   where neg = if n < 0 then negate else id
  



-- module SquareDigit where

-- import Data.Char

-- squareDigit :: Int -> Int
-- squareDigit = read . concatMap sq . show where
--     sq c
--     | isDigit c = show . (^2) . digitToInt $ c
--     | otherwise = [c]





-- module SquareDigit where
-- import Data.Char

-- squareDigit :: Int -> Int
-- squareDigit x
--     | x < 0  = -(squareDigit (-x))
--     | x < 10 = x*x
--     | otherwise = if r <= 3
--                 then (squareDigit q)*10 + r*r 
--                 else (squareDigit q)*100 + r*r
--                 where (q, r) = x `quotRem` 10




-- module SquareDigit where
-- import Data.Char

-- squareDigit :: Int -> Int
-- squareDigit = read . aux . show
--   where aux [] = []
--         aux (d:ds) = squareSingle d ++ aux ds
        
-- squareSingle s = case s of
--   '0' -> "0"
--   '1' -> "1"
--   '2' -> "4"
--   '3' -> "9"
--   '4' -> "16"
--   '5' -> "25"
--   '6' -> "36"
--   '7' -> "49"
--   '8' -> "64"
--   '9' -> "81"
--   '-' -> "-"



-- module SquareDigit where
-- import Data.Char

-- squareDigit :: Int -> Int
-- squareDigit n = read $ concat $ map squareChar $ show n
--    where squareChar c
--            | isDigit c = show $ (ord c - 48)^2
--            | otherwise = [c]