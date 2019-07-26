module Codewars.G964.Getmiddle where


import Data.List

nthOfList :: [a] -> Int -> [a]
nthOfList [] _       = []
nthOfList xs 0       = []
nthOfList (xs) (n) =  drop (n-1) (take n xs) ++ nthOfList (drop n xs) n



getMiddle :: [Char] -> [Char] 
getMiddle x = if length x == odd then (!! n) x
              else error "nothing"
            where n = 2
-- head $ take ((length x) `div` 2) x 