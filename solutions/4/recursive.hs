printRow [] = "|"
printRow ((x, y) : xs) = "|" ++ printField x y ++ printRow xs
