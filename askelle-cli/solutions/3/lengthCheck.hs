printField :: Int -> String -> String
printField nr text
    | length text == nr = text
    | all isDigit text = replicate (nr - length text) ' ' ++ text
    | otherwise = text ++ replicate (nr - length text) ' '
