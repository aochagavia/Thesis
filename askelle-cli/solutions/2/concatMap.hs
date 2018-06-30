printLine xs = concatMap (\x -> '+' : replicate x '-') xs ++ ['+']
