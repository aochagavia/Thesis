printField :: Int -> String -> String
printField n xs
    | all isDigit xs = padding ++ xs
    | otherwise = xs ++ padding
    where padding = replicate (n - length xs) ' '
