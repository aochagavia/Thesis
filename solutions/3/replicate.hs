printField :: Int -> String -> String
printField n xs | b = replicate (n - length xs) ' '  ++ xs
                | otherwise = xs ++ replicate (n - length xs) ' '
            where b = all isDigit xs
