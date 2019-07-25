module Isogram where

import Data.List 
import Data.Char 

isIsogram :: String -> Bool
isIsogram str = map toUpper str == (nub $ map toUpper str)


-- -- alternative solutions

-- module Isogram where
-- import Data.Char (toLower)
-- import Data.List

-- isIsogram :: String -> Bool
-- isIsogram str = null $ map toLower str \\ ['a'..'z']



-- module Isogram where

-- import Data.Char (toLower)
-- import qualified Data.Set as S

-- isIsogram :: String -> Bool
-- isIsogram = (==) <$> length <*> S.size . S.fromList . map toLower

-- module Isogram where

-- import Data.Char
-- import Data.List

-- isIsogram :: String -> Bool
-- isIsogram x = y == nub y
--     where y = map toLower x:


-- module Isogram where
-- import Data.Char

-- isIsogram :: String -> Bool
-- isIsogram [] = True
-- isIsogram (x:xs)
--     | toLower x `elem` [toLower n| n <- xs] = False
--     | otherwise = isIsogram xs