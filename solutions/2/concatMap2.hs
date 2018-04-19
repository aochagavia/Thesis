printLine xs = '+' : concatMap (\x2 -> replicate x2 '-' ++ "+") xs
