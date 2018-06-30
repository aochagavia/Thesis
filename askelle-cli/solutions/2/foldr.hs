printLine = foldr (\x xs -> '+' : replicate x '-' ++ xs) "+"
