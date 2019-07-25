

module FindShortest where
find_shortest :: String-> Integer
find_shortest l1  = toInteger $ minimum $ map length $ words l1

{- alternative methods by others

module FindShortest where

import Data.List

find_shortest :: String -> Integer
find_shortest = minimum . fmap genericLength . words


module FindShortest where
import Data.List
find_shortest :: String -> Integer
find_shortest xs =
  head (sort (map genericLength (words xs)))


module FindShortest where
find_shortest :: String -> Integer
find_shortest str = foldr min (toInteger (length str)) (map toInteger (map length (words str)))


module FindShortest where
find_shortest :: String -> Integer
find_shortest =
    fromIntegral . snd . foldr1 shorter . map (\w -> (w, length w)). words

shorter :: (String, Int) -> (String, Int) -> (String, Int)
l@(_, ll) `shorter` r@(_, lr)  = if ll <= lr then l else r
  


module FindShortest where
import Data.List.Split
find_shortest :: String -> Integer
find_shortest str = toInteger (minimum lengths) 
  where 
    splittedStr = splitOn " " str
    lengths = map (length) splittedStr


module FindShortest where

find_shortest :: String -> Integer
find_shortest x = toInteger (length (shortest_word (words x)))

shortest_word :: [String] -> String
shortest_word [x] = x
shortest_word [x, y] = if length x < length y then x else y
shortest_word (x:xs) = shortest_word [x, (shortest_word xs)]


-}