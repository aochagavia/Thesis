module Exercise5 where

import Data.Char (isDigit)
import Data.Function (fix)
import Data.List (intercalate, transpose)
import Test.QuickCheck
import SampleTables

-- Note: emptyTable, t1, t2 and t3 are all defined in Correctness.hs
properties :: ([[String]] -> [Int]) -> [Property]
properties f = [p t1 f, p t2 f, p t3 f]

-- Precondition: the list is a well-formed table
columnWidths = map (maximum . map length) . transpose

p :: [[String]] -> ([[String]] -> [Int]) -> Property
p table f = counterexample print $ f table == columnWidths table
    where
        print = unlines [ "Expected: " ++ show (columnWidths table)
                        , "Output: " ++ show (f table)]
