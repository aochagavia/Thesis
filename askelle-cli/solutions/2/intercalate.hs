printLine x = "+" ++ intercalate "+" (map (flip replicate '-' ) x) ++ "+"
