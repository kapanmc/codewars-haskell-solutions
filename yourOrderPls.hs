
module Codewars.Kata.YourOrderPlease where

import Data.Char
import Data.List



yourOrderPlease :: String -> String
yourOrderPlease txt = unwords $ snd $ unzip $ sort $ zip (map numbersInside $ words txt) (words txt) 

numbersInside wrd = [ x | x <- wrd, x `elem` ['1'..'9']]



txttest =    "is2 Thi1s T4est 3a" 
txttest2 = "4of Fo1r pe6ople g3ood th5e the2"
wrd1 = head $ words txttest


-- excludeSomeNumbers = [ x | x <- [10..20], x /= 13, x /= 15, x /= 19]

-- midFunc txt = map numbersInside $ words txt

-- tempFunc txt = unwords $ snd $ unzip $ sort $ zip (map numbersInside $ words txt) (words txt) 

{- Alternatives


module Codewars.Kata.YourOrderPlease where
import Data.List 
import Data.Char
import Data.Ord

yourOrderPlease :: String -> String
yourOrderPlease = unwords . sortBy (comparing (head . filter isDigit)) . words





module Codewars.Kata.YourOrderPlease where

import Data.List (sortOn, find)
import Data.Char (isDigit)

yourOrderPlease :: String -> String
yourOrderPlease = unwords . sortOn (find isDigit) . words





module Codewars.Kata.YourOrderPlease where
import Data.Char ( isNumber )
import Data.List ( find, sortBy )
import Data.Ord  ( comparing )

yourOrderPlease :: String -> String
yourOrderPlease = unwords . sortBy (comparing $ find isNumber) . words




import Data.List
import Data.Ord
import Data.Char

yourOrderPlease :: String -> String
yourOrderPlease [] = []
yourOrderPlease s = unwords $ sortBy (comparing $ filter isDigit) $ words s



import Data.List (sortBy, find)
import Data.Char (isDigit)
import Data.Maybe (fromJust)

yourOrderPlease :: String -> String
yourOrderPlease x = unwords $ sortBy (\a b -> compare (getDigit a) (getDigit b)) (words x)
    where getDigit = fromJust . find isDigit



module Codewars.Kata.YourOrderPlease where
import Data.Char (ord)

split :: String -> [String]
split "" = []
split [x] = [[x]]
split (x:' ':ls) = [x]:(split ls)
split (x:ls) = 
    (x:(head res)):(tail res) 
    where res = split ls 

getId :: String -> Int
getId "" = 0
getId (x:ls) = if elem x ['1'..'9'] then (ord x - ord '1')
                                    else getId ls

min' :: [String] -> Int
min' [] = error "You shouldn't ask this!" 
min' [x] = 0
min' (x:ls) =
    if getId (ls!!tail_id) < getId x then tail_id+1 else 0 
    where tail_id = min' ls

getOutById :: [String] -> Int -> [String]
getOutById ls i = (init (take (i+1) ls)) ++ (drop (i+1) ls)

sort :: [String] -> [String]
sort [] = []
sort [x] = [x]
sort (x:ls) = 
    if (getId x) < (getId tail_min_item)
        then x:(sort ls) 
        else tail_min_item : sort (x:(getOutById ls tail_min_id))
    where tail_min_id = min' ls
          tail_min_item = ls!!tail_min_id

join :: [String] -> String
join [] = ""
join [x] = x
join (x:ls) = x ++ " " ++ join ls

yourOrderPlease :: String -> String
yourOrderPlease = join.sort.split    


import Data.List (sort)
import Data.Char (isDigit)

yourOrderPlease :: String -> String
yourOrderPlease input = 
  let input' = words input
      pos :: [Int]
      pos = map (read . filter isDigit) input'
  in unwords . map snd . sort . zip pos $ input'






module Codewars.Kata.YourOrderPlease where
import Data.List
import Data.Char

yourOrderPlease :: String -> String
yourOrderPlease x = intercalate " " (sortBy myComp (words x))

myComp :: String -> String -> Ordering
myComp str1 str2 = compare (f str1) (f str2)

f :: (Num a, Read a) => String -> a
f [] = 100
f (x:xs) = if isNumber x then read [x] else f xs





module Codewars.Kata.YourOrderPlease where
import Data.List 

extractNum :: String -> Int
extractNum = read . filter (`elem` ['0'..'9']) . show

comp :: String -> String -> Ordering
comp f s = compare a b
  where a = extractNum f 
        b = extractNum s

yourOrderPlease :: String -> String
yourOrderPlease = unwords . sortBy comp . words 



import Data.Char
import Data.List

yourOrderPlease :: String -> String
yourOrderPlease xs = u
  where w = words xs
        f = map (filter isDigit) w
        z = zip f w
        s = sortOn fst z
        u = unwords $ map snd s





import Data.Char
import Data.List

yourOrderPlease :: String -> String
yourOrderPlease = unwords . map snd . sortOn fst . map (\s -> ((read :: String -> Int) $ filter isDigit s, s)) . words 





import Data.List

yourOrderPlease :: String -> String
yourOrderPlease = unwords . sortOn (head . filter (`elem` ['1'..'9'])) . words


import Data.List
import Data.Char

yourOrderPlease :: String -> String
yourOrderPlease "" = ""
yourOrderPlease x = unwords $ sortBy stringOrder $ words x
  where stringOrder a b = (getOrder a) `compare` (getOrder b)
        getOrder [] = 0
        getOrder (x:xs) = if isDigit x then digitToInt x else getOrder xs

-}