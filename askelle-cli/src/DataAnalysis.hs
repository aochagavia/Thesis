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
