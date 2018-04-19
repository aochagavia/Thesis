project columns table@(header:_) =
    let tt = transpose table
        is = mapMaybe (`elemIndex` header) columns
     in if null is then
            replicate (length table) []
        else
            transpose (map (tt !!) is)
