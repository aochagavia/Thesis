module Exercise3 where

import Data.Char (isDigit)
import Data.Function (fix)
import Test.QuickCheck

properties :: (Int -> String -> String) -> [Property]
properties f = [propModel f]

-- Precondition: n >= length s
printField :: Int -> String -> String
printField n xs | b = replicate (n - length xs) ' '  ++ xs
                | otherwise = xs ++ replicate (n - length xs) ' '
            where
                b = all isDigit xs


-- Input is a string
propModel f = property $ \n s -> n >= length s ==> (f n s == printField n s)
