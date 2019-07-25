module UniqueInOrder (uniqueInOrder) where


import qualified Data.List as List

uniqueInOrder :: Eq a => [a] -> [a]
uniqueInOrder [] = []
uniqueInOrder [x] = [x]
uniqueInOrder (x:xs) = map head $ List.group (x:xs)


{-

https://mail.haskell.org/pipermail/haskell-cafe/2009-March/057769.html

uniqueInOrder :: (Eq a) => [a] -> [a]
uniqueInOrder []            =  []
uniqueInOrder (x : [])      =  [x]
uniqueInOrder (x : xx : xs) =  if x == xx then uniqueInOrder (x : xs) 
else x : uniqueInOrder (xx : xs)

--LIST Comprehension & half works:
-- uniqueInOrder (x:xs) = ((\l -> [ x  | (x,y) <- zip l $ (tail l) , x /= y]) (x:xs)) 


import Data.List

uniqueInOrder :: Eq a => [a] -> [a]
uniqueInOrder = map head . group

uniqueInOrder :: Eq a => [a] -> [a]
uniqueInOrder [] = []
uniqueInOrder (s:[]) = [s]
uniqueInOrder (a:b:xs) = 
  if a == b then 
    uniqueInOrder (a:xs) 
  else 
    a : uniqueInOrder (b:xs)

uniqueInOrder [] = []
uniqueInOrder [x] = [x]
uniqueInOrder a = if head a == head (tail a)
                  then uniqueInOrder (tail a)
                  else [head a] ++ uniqueInOrder (tail a)    



import Data.List

helper acc x 
  | acc == []     = [x]
  | x == last acc = acc
  | otherwise     = acc ++ [x]

uniqueInOrder list = foldl' (helper) [] list



-}


