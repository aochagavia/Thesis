module Exercise7 where

import Data.Char (isDigit)
import Data.Function (fix)
import Data.List (intercalate, transpose, elemIndex)
import Test.QuickCheck
import SampleTables

-- Note: emptyTable, t1, t2 and t3 are all defined in Correctness.hs
properties :: (String -> String -> [[String]] -> [[String]]) -> [Property]
properties f = concat [p t1 f, p t2 f, p t3 f]

-- Precondition: the list is a well-formed table
select column value table@(header:rows)
    = let columnNumber = elemIndex column header
      in case columnNumber of
        Nothing -> table
        Just found -> header : filter (\f -> f !! found == value) rows

p :: [[String]] -> (String -> String -> [[String]] -> [[String]]) -> [Property]
p table@(header:rows) f = map prop $ genInputs header rows
    where
        prop (colName, rowVal) = counterexample (print colName rowVal) $ f colName rowVal table == select colName rowVal table
        print colName rowVal =
            unlines [ "Table: " ++ show table
                    , "Col: " ++ show colName
                    , "Val: " ++ show rowVal
                    , "Output: " ++ show (f colName rowVal table)
                    , "Expected: " ++ show (select colName rowVal table)
                    ]


genInputs :: [String] -> [[String]] -> [(String, String)]
genInputs header rows =
    let cols = transpose rows
    in ("non-existing-col", "Alice")
        : (head header, "non-existing-val")
        : (concatMap (\(h, cs) -> map (\c -> (h, c)) cs) $ zip header cols)

-- Ideas: for each column, try selecting all elements
