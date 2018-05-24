module Exercise8 where

import Control.Exception (SomeException, catch, evaluate)
import Data.Char (isDigit)
import Data.Function (fix)
import Data.List (intercalate, transpose, elemIndex, elemIndices, find)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Test.QuickCheck
import System.IO.Unsafe (unsafePerformIO)

import SampleTables

-- Note: emptyTable, t1, t2 and t3 are all defined in Correctness.hs
properties :: ([String] -> [[String]] -> [[String]]) -> [Property]
properties f = concat [p t1 f, p t2 f, p t3 f]

-- Precondition: the list is a well-formed table
project colNames table@(header:_)
    = transpose $ map getCol $ mapMaybe getColIndex colNames
      where
        getColIndex colName = elemIndex colName header
        getCol index = transpose table !! index

-- Slightly different than the previous one
project'  :: [String] -> [[String]] -> [[String]]
project' colNames table@(header:_) =
        map (\row -> map (getField row) (mapMaybe getColIndex colNames)) table
    where
        getColIndex colName = elemIndex colName header
        getField row index = row !! index

p :: [[String]] -> ([String] -> [[String]] -> [[String]]) -> [Property]
p table@(header:_) f = map prop $ genInputs header
    where
        prop colNames =
            let output = f colNames table
                expected = project colNames table
                expected' = project' colNames table
            in if null expected
                then counterexample (print2 colNames) $ output == expected || output == expected'
                else counterexample (print1 colNames) $ output == expected

        print1 colNames =
            unlines [ "Table: " ++ show table
                    , "Cols: " ++ show colNames
                    , "Output: " ++ catchError (show (f colNames table))
                    , "Expected: " ++ show (project colNames table)
                    ]
        print2 colNames =
            unlines [ "Table: " ++ show table
                    , "Cols: " ++ show colNames
                    , "Output: " ++ catchError (show (f colNames table))
                    , "Expected: " ++ show (project colNames table)
                    , "Expected (alternative): " ++ show (project' colNames table)
                    ]
        catchError :: String -> String
        catchError x = unsafePerformIO $ catch (evaluate x) ignoreEx
        ignoreEx :: SomeException -> IO String
        ignoreEx _ = return "<exception>"

-- Test on single column names and combinations of two and three
genInputs :: [String] -> [[String]]
genInputs header = concatMap (flip subsequencesOfSize header) [1..3]
        ++ [ [] -- No projection at all
           , ["non-existent column"]
           ]
-- Taken from https://stackoverflow.com/questions/21265454
subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize n xs = let l = length xs
                          in if n>l then [] else subsequencesBySize xs !! (l-n)
    where
    subsequencesBySize [] = [[[]]]
    subsequencesBySize (x:xs) = let next = subsequencesBySize xs
                                in zipWith (++) ([]:next) (map (map (x:)) next ++ [[]])
