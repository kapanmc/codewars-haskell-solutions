
import Data.Char
import Data.List


isPangram :: String -> Bool
isPangram [] = False
isPangram str = null $ alphabet \\ (map toLower str)
          where alphabet = ['a'..'z']



-- testCase = "The quick brown fox jumps over the lazy dog"
