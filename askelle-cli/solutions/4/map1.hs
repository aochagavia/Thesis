printRow xs = "|" ++ intercalate "|" (map (uncurry printField) xs) ++ "|"
