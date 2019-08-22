module MultNumAsStrings where


import Data.Char (intToDigit, digitToInt)

-- | mulitply two numbers as strings
multiply :: String -> String -> String
multiply xs ys = show $ read xs * read ys


{- ALTERNATIVE imperative style


module MultNumAsStrings where

import Data.Char (intToDigit, digitToInt)
import Data.List (transpose)
import Data.Maybe (catMaybes)

type Digits = [Int]

toDigits :: String -> Digits
toDigits = reverse . map digitToInt

fromDigits :: Digits -> String
fromDigits = reverse . map intToDigit

-- | mulitply two numbers as strings
multiply :: String -> String -> String
multiply (toDigits -> xs) (toDigits -> ys) 
  = pruneLeading $ fromDigits $ sum' $ map (\d -> mul' d ys) xs

pruneLeading :: String -> String
pruneLeading ('0':a:as) = pruneLeading (a:as)
pruneLeading as = as

-- | Do a calculation and carry the one
carryOp :: (a -> Int) -> [a] -> Digits
carryOp f = go 0 where
  go 0 [] = []
  go m [] = [m]
  go m (n:ns) = digit : go carry ns where
    result = f n + m
    digit = result `rem` 10
    carry = result `div` 10

-- | Multiply a multi-digit number by a single digit carrying the one
mul' :: Int -> Digits -> Digits
mul' digit = carryOp (digit *)

-- | Sum multiple numbers together with offset and carrying the one
sum' :: [Digits] -> Digits
sum' = carryOp sum . steppingTranspose

steppingTranspose :: [[a]] -> [[a]]
steppingTranspose = trim . transpose . prefix where
  prefix = zipWith (\i l -> replicate i Nothing ++ map Just l) [0..]
  trim = map catMaybes


-}