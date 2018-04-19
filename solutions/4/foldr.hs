printRow :: [(Int, String)] -> String
printRow = foldr (\(x,y) z -> "|" ++ (printField x y) ++ z ) "|"
