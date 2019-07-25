

sumArray :: Maybe [Int] -> Int
sumArray Nothing = 0
sumArray (Just []) = 0
sumArray (Just [x]) = 0
sumArray (Just xs) = sum xs - maximum xs - minimum xs

--https://www.codewars.com/kata/576b93db1129fcf2200001e6/solutions/haskell

{-
Alternate solutions

import Data.List

sumArray :: Maybe [Int] -> Int
sumArray (Nothing) = 0 :: Int 
sumArray (Just []) = 0 :: Int 
sumArray (Just [_]) = 0 :: Int 
sumArray (Just xs) = sum $ tail $ init $ sort xs 



import Data.List 
import Data.Maybe
sumArray :: Maybe [Int] -> Int
sumArray xs = sum $ drop 1 $ reverse $ drop 1 $ sort $ concat $ maybeToList xs


sumArray :: Maybe [Int] -> Int
sumArray Nothing = 0
sumArray (Just xs) = getSum xs

getSum :: [Int] -> Int
getSum [] = 0
getSum [x] = 0
getSum xs = total - minEl - maxEl
  where
    total = sum xs
    minEl = minimum xs
    maxEl = maximum xs

-}