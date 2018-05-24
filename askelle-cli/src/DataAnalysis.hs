module DataAnalysis where

import Data.List (foldl')

summarize :: String -> Int -> IO () --String -> IO ()
summarize resultSet exNumber = do
    -- For each file without an extension, read the text,
    file <- readFile $ "../data/" ++ resultSet ++ "/exercise" ++ show exNumber
    summarizeGroups $ linesToGroups $ lines file


summarizeGroups :: [[String]] -> IO ()
summarizeGroups gs = do
    putStrLn $ "Number of groups: " ++ show (length gs)
    putStrLn $ "Groups ordered by size: " ++ show (map length gs)

linesToGroups :: [String] -> [[String]]
linesToGroups = filter (/= []) . foldl' (\acc@(h:t) line -> if line == "" then [] : acc else (line : h) : t) [[]]

exampleData :: [String]
exampleData =
    [ "4045483"
    , "5142489"
    , "3412483"
    , "" -- End of prevous group, start of new one
    , "8912378"
    , "3489709"
    , ""
    , ""
    ]

{-
        bucketize :: [Int] -> [Int]
        bucketize xs = let b1 = bucket (== 1)
                           b2 = bucket (== 2)
                           b3 = bucket (\x -> 2 < x && x <= 4)
                           b4 = bucket (\x -> 4 < x && x <= 8)
                           b5 = bucket (\x -> 8 < x && x <= 16)
                           b6 = bucket (\x -> 16 < x && x <= 32)
                           b7 = bucket (\x -> 32 < x && x <= 64)
                           b8 = bucket (\x -> 64 < x)
                       in  [b1, b2, b3, b4, b5, b6, b7, b8]
            where
                bucket :: (Int -> Bool) -> Int
                bucket f = length $ filter f xs
        -}
