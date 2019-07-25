square :: (Num a) => a -> a
square x = x * x

squareSum :: (Num a) => [a] ->  a
squareSum [] = error "empty list "
squareSum [x] = square x
squareSum (x:xs) = sum $ map square (x:xs)


-- alternate solutions of others

-- https://www.codewars.com/kata/515e271a311df0350d00000f/solutions/solutions
{-


squareSum :: [Integer] -> Integer
squareSum = sum . map (^2):

squareSum :: [Integer] -> Integer
squareSum [] = 0
squareSum (x:xs) = x ^ 2 + squareSum xs


squareSum :: [Integer] -> Integer
squareSum = foldr (\x s -> x*x + s) 0

-}
squareSum' :: [Integer] -> Integer
squareSum' = foldr ((+) . (^2)) 0