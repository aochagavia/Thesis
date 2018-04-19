printLine []     = ['+']
printLine (x:xs) = ['+'] ++ replicate x '-' ++ printLine xs
