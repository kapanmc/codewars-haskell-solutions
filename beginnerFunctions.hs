
import Data.List


doubleMe x = x + x
doubleUs x y = x*2 + y*2 

doubleSmallNumber x = if x > 100
                        then x
                        else x*2 


doubleSmallNumber' x = (if x > 100 then x else x*2) +1 

lostNumbers = [4,8,15,16,23,42] 
conanCharList = "Conan O'Brian"


listComprehensionEvenNumbers = [ x*2 | x <- [1..10]]

listComprehensionEvenNumbersBiggerThan12 = [ x*2 | x <- [1..10], x*2 >= 12]

--boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]:

excludeSomeNumbers = [ x | x <- [10..20], x /= 13, x /= 15, x /= 19]

rmvSpc:: String -> String
rmvSpc str = [x | x <- str, x /= ' ']

noSpace :: String -> String
noSpace = filter (/=' ')
-- alternative methods
{-
import Data.Char (isSpace)
noSpace :: String -> String
noSpace = filter (not . isSpace)


noSpace :: String -> String
noSpace [] = []
noSpace (x:xs) = if x == ' ' then noSpace xs else  [x]  ++ (noSpace xs)



noSpace :: String -> String
noSpace = foldl (\x y -> if [y] == " " then x else x ++ [y]) ""
-}

noSpace' :: String -> String
noSpace' = concat . words

noSpace2 :: String -> String
noSpace2 = filter (`notElem` " ")

length' xs = sum [1 | _ <- xs]   

removeNonUppercase st = [ c | c <- st, c `elem` ['A'.. 'Z']]

xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]  
removeOdds xxs = [ [ x | x <- xs, even x ] | xs <- xxs]  

factorial :: Integer -> Integer
factorial n = product [1..n]


length2 ::  (Num b) => [a] -> b
length2 [] = 0 
length2 (_:xs) = 1 + length2 xs


sum':: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs


max':: (Ord a) => a -> a -> a
max' a b 
    | a > b  = a
    | otherwise = b


compare' :: (Ord a) => a -> a -> Ordering
compare' a b 
    | a > b = GT
    | a == b = EQ
    | otherwise = LT



bodyMassIndex :: (RealFloat a) => a -> a -> String
bodyMassIndex weight height
    | bodyMassIndex <= skinny = "underweight"
    | bodyMassIndex <= normal = "normal"
    | bodyMassIndex <= fat = "fatty" 
    | otherwise = "obese"
    where bodyMassIndex = weight / height^2
          skinny = 18.5
          normal = 25.0
          fat = 30.0 





cylinder :: (RealFloat a) => a -> a -> a 
cylinder r h =
    let sideArea = 2 * pi * r * h 
        topArea = pi * r^2
    in  sideArea + 2 * topArea
    
    
 
    
-- import Data.List   // en yukarida import edildi

nthOfList :: [a] -> Int -> [a]
nthOfList [] _       = []
nthOfList xs 0       = []
nthOfList (xs) (n) =  drop (n-1) (take n xs) ++ nthOfList (drop n xs) n