module Codewars.G964.Longest where

import Data.List

longest :: [Char] -> [Char] -> [Char]
longest s1 s2 = sort $ nub $ s1 ++ s2


-- ALTERNATIVE METHODS

-- module Codewars.G964.Longest where

-- import Data.List

-- longest :: [Char] -> [Char] -> [Char]
-- longest s1 s2 = map head . group . sort $ s1 ++ s2


-- module Codewars.G964.Longest where
-- import qualified Data.Set as Set

-- longest :: [Char] -> [Char] -> [Char]
-- longest s1 s2 = Set.toList . Set.fromList $ s1 ++ s2


-- module Codewars.G964.Longest where

-- longest :: [Char] -> [Char] -> [Char]
-- longest s1 s2 = a4
--     where
--         a1 = [ (x, c) | x<-['a'..'z'], let c = (length.filter (==x)) s1 ]
--         a2 = [ (x, c) | x<-['a'..'z'], let c = (length.filter (==x)) s2 ]
--         a3 = filter (\x -> (snd x /= 0)) $ map (\(x,y) -> (fst x, max (snd x) (snd y))) (zip a1 a2)
--         a4 = map (\(x, y) -> x) a3