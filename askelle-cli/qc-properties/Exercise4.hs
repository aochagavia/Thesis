module Exercise4 where

import Data.Char (isDigit)
import Data.Function (fix)
import Data.List (intercalate)
import Test.QuickCheck

properties :: ([(Int, String)] -> String) -> [Property]
properties f = [propModel f]

-- Precondition: n >= length s
printField :: Int -> String -> String
printField n xs | b = replicate (n - length xs) ' '  ++ xs
                | otherwise = xs ++ replicate (n - length xs) ' '
            where
                b = all isDigit xs

-- Precondition: n >= length s
printRow :: [(Int, String)] -> String
printRow = foldr (\(x,y) z -> "|" ++ (printField x y) ++ z ) "|"

-- Input is a string
propModel :: ([(Int, String)] -> String) -> Property
propModel f = property $ \xs -> let tups = toTuples xs in (f tups == printRow tups)

-- Custom arbitrary datatype, otherwise QuickCheck gives up

-- Generate a list of String
-- Then, for each string in the list, generate a number between length s and length s + 4

data IntStringPair = IntStringPair (Int, NonEmptyList Char) deriving (Show)

toTuples :: NonEmptyList IntStringPair -> [(Int, String)]
toTuples (NonEmpty xs) = map toTuple xs

toTuple :: IntStringPair -> (Int, String)
toTuple (IntStringPair (x, NonEmpty s)) = (x, s)

instance Arbitrary IntStringPair where
    arbitrary = do
        NonEmpty string <- arbitrary
        int <- choose (length string, length string + 4)
        return $ IntStringPair (int, NonEmpty string)
