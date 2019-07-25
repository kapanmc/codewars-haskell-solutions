module ASCII where

    import Data.Char (ord)
    getASCII :: Char -> Int
    getASCII a = ord a 

--alternative solutions

-- module ASCII where

--     getASCII :: Char -> Int
--     getASCII = fromEnum



-- module ASCII where

-- import Data.Char (ord)

-- getASCII :: Char -> Int
-- getASCII = ord

-- module ASCII where
--     import Data.Char
--     getASCII :: Char -> Int
--     getASCII c = if (k > 0) || (k <= 128) then k else error "y no codez?" where k =fromEnum c