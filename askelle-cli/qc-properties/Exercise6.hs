module Exercise6 where

import Data.Char (isDigit, toUpper)
import Data.Function (fix)
import Data.List (intercalate, transpose)
import Test.QuickCheck
import SampleTables

-- Note: emptyTable, t1, t2 and t3 are all defined in Correctness.hs
properties :: ([[String]] -> [String]) -> [Property]
properties f = [p t1 f, p t2 f, p t3 f]

printLine :: [Int] -> String
printLine xs = "+" ++ intercalate "+" (map (flip replicate '-' ) xs) ++ "+"

-- Precondition: n >= length s
printField :: Int -> String -> String
printField n xs | b = replicate (n - length xs) ' '  ++ xs
                | otherwise = xs ++ replicate (n - length xs) ' '
            where
                b = all isDigit xs

-- Precondition: n >= length s
printRow :: [(Int, String)] -> String
printRow = foldr (\(x,y) z -> "|" ++ (printField x y) ++ z ) "|"

-- Precondition: the list is a well-formed table
columnWidths = map (maximum . map length) . transpose

printTable :: [[String]] -> [String]
printTable table@(header:rows)
    = line : headerLine : line : otherlines ++ line : [] where
    line = printLine width
    width = columnWidths table
    headerLine = printRow (zip width (map (map (toUpper)) header))
    otherlines = map (printRow . zip width) rows

p :: [[String]] -> ([[String]] -> [String]) -> Property
p table f = counterexample print $ f table == printTable table
    where print = unlines [ "Expected: " ++ unlines (printTable table)
                          , "Output: " ++ unlines (f table)]
