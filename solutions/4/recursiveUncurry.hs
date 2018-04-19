printRow :: [(Int, String)] -> String
printRow [] = "|"
printRow (x:xs) = "|" ++ uncurry printField x ++ printRow xs
