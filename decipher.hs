
module Kata (decipherThis) where 

import Data.Char
import Data.List

testStr = "72olle 103doo 100ya"
str2 = "72olle"
str3 = "100ya"
str4 = "103doo"
str5 = "115te"
str6 = "82yade"


decipherThis :: String -> String
decipherThis "" = ""
decipherThis s = unwords $ map swap $ map replaceIntToChar (words s)

firstDigToChar str = chr $ read $ filter (isNumber) str
-- firstDigToChar str = chr $ read $ take 3 str

replaceIntToChar str = firstDigToChar str : drop (length $ filter (isNumber) str)  str
-- decipher n = head $ tail n


-- swap a b = map (\x -> if x == a then b else if x == b then a else x)


-- swap _ _ [] = []
-- swap n m (x:xs)
--   | n == x = m : (swap n m xs)
--   | m == x = n : (swap n m xs)
--   | otherwise = x : (swap n m xs)


-- decipher' s = swap (head$ tail $ replaceIntToChar s ) (last $ replaceIntToChar s) (replaceIntToChar s)
swap' :: String -> String
swap' [] = [] 
swap' [x] = [x]
swap' [x,y] = [x,y]
swap' s = head s : last s : (drop 2 $ init s) ++ [head $ tail s]

decipherThis' s = unwords $ map swap' $ map replaceIntToChar (words s)




swap :: String -> String
swap [] = []
swap [x] = [x]
swap [x,y] = [x,y]
swap (n:ns:nss) = n : [last nss] ++ init nss ++ [ns]
-- swap (n:ns:nss) 
--     | length (n:ns:nss) == 1 = [n] 
--     | length (n:ns:nss) == 2 = (n:ns) 
--     | otherwise = n : [last nss] ++ init nss ++ [ns]


--------------------------------------------------------------
-- ALTERNATIVES

{-

module Kata (decipherThis) where 

import Data.Char (isDigit, chr)

d1 :: String -> String
d1 [] = []
d1 x = [toEnum (read x :: Int) :: Char]

d2 :: String -> String
d2 [] = []
d2 [x] = [x]
d2 xs = last xs : (tail $ init xs) ++ [head xs]

decipherThis :: String -> String
decipherThis s = unwords
  $ map (\(x,y) -> d1 x ++ d2 y)
  $ map (span isDigit)
  $ words s




module Kata (decipherThis) where 
import Data.Char

decipherThis :: String -> String
decipherThis message = unwords $ map decipherWord (words message)

decipherWord :: String -> String
decipherWord s = (chr x) : (swapFirstLast y)
                 where (x, y) = split s

split :: String -> (Int, String)
split s = (read (takeWhile isDigit s) :: Int, dropWhile isDigit s)

swapFirstLast :: String -> String
swapFirstLast [] = []
swapFirstLast [x] = [x]
swapFirstLast (x:y:[]) = y:x:[]
swapFirstLast (x:xs) = (last xs) : (init xs) ++ [x]


module Kata (decipherThis) where

import Data.Char (chr, isDigit)
import Data.List (groupBy)

decipher :: String -> String
decipher s =
  chr (read charCode) : swapFirstLast rest
  where
    (charCode, rest) = span isDigit s

swapFirstLast :: [a] -> [a]
swapFirstLast [] = []
swapFirstLast [x] = [x]
swapFirstLast (x:xs) = last xs : init xs ++ [x]

decipherThis :: String -> String
decipherThis = unwords . map decipher . words



module Kata (decipherThis) where 
import Data.Char (isDigit, chr)
import Data.List (words, unwords, init, last)
decipherThis :: String -> String
decipherThis  =  unwords . map delit . words
  where delit :: String -> String
        delit str = delitel [] str
          where delitel acc [] = [oneCode acc]
                delitel acc (z:zx) = if isDigit z
                                       then delitel (acc ++ [z]) zx
                                       else [oneCode acc] ++ reverSec (z:zx)
                reverSec [] = []
                reverSec (x:[]) = x:[]
                reverSec (x:xs) = [last xs] ++ init xs ++ [x]
                oneCode y = chr (read y :: Int)


module Kata (decipherThis) where 

import Data.Char (chr, isDigit)

decipherThis :: String -> String
decipherThis = unwords . map decipherWord . words 
        
decipherWord :: String -> String
decipherWord w
  | n == 0    = [c']
  | n == 1    = c' : s
  | otherwise = c' : [last s] ++ (tail $ init s) ++ [head s]
  where (c, s) = span isDigit w
        c' = chr $ read c
        n = length s


-}





