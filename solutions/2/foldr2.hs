printLine = foldr ((++) . (\ x -> "+" ++ replicate x '-')) "+"
